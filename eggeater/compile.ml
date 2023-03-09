open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors

type 'a envt = (string * 'a) list

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet (binds, body, _) -> List.for_all (fun (_, e, _) -> is_anf e) binds && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e

and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;

let const_true = HexConst 0xFFFFFFFFFFFFFFFFL

let const_false = HexConst 0x7FFFFFFFFFFFFFFFL

let bool_mask = HexConst 0x8000000000000000L

let bool_tag = 0x0000000000000007L

let bool_tag_mask = 0x0000000000000007L

let num_tag = 0x0000000000000000L

let num_tag_mask = 0x0000000000000001L

let pair_tag = 0x0000000000000001L

let err_COMP_NOT_NUM = 1L

let err_ARITH_NOT_NUM = 2L

let err_LOGIC_NOT_BOOL = 3L

let err_IF_NOT_BOOL = 4L

let err_OVERFLOW = 5L

let err_GET_NOT_TUPLE = 6L

let err_GET_LOW_INDEX = 7L

let err_GET_HIGH_INDEX = 8L

let err_NIL_DEREF = 9L
let err_INDEX_NOT_NUMBER = 10L

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]

let heap_reg = R15

let scratch_reg = R11

let nil_val = HexConst 0x001L

(* You may find some of these helpers useful *)
let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y, v) :: rest ->
      if y = x then
        v
      else
        find rest x
;;

let rec find_opt ls x =
  match ls with
  | [] -> None
  | (y, v) :: rest ->
      if y = x then
        Some v
      else
        find_opt rest x
;;

let count_vars e =
  let rec helpA e =
    match e with
    | ALet (_, bind, body, _) -> 1 + max (helpC bind) (helpA body)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf (_, t, f, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in
  helpA e
;;

let rec replicate x i =
  if i = 0 then
    []
  else
    x :: replicate x (i - 1)
;;

let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
  | [] -> None
  | (DFun (fname, _, _, _) as d) :: ds_rest ->
      if name = fname then
        Some d
      else
        find_decl ds_rest name
;;

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
  | [] -> false
  | x :: xs -> elt = x || find_one xs elt
;;

let rec find_dup (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | [x] -> None
  | x :: xs ->
      if find_one xs x then
        Some x
      else
        find_dup xs
;;

type funenvt = call_type envt

let initial_fun_env : funenvt =
  [ (* call_types indicate whether a given function is implemented by something in the runtime,
       as a snake function, as a primop (in case that's useful), or just unknown so far *)
    ("print", Native);
    ("input", Native);
    ("equal", Native)]
;;

let rename_and_tag (p : tag program) : tag program =
  let rec rename (env : string envt) p =
    match p with
    | Program (decls, body, tag) ->
        let rec addToEnv funenv decl =
          match decl with
          | DFun (name, _, _, _) -> (name, Snake) :: funenv
        in
        let initial_funenv = List.map (fun (name, ct) -> (name, ct)) initial_fun_env in
        let funenv = List.fold_left addToEnv initial_funenv decls in
        Program (List.map (helpD funenv env) decls, helpE funenv env body, tag)
  and helpD funenv env decl =
    match decl with
    | DFun (name, args, body, tag) ->
        let newArgs, env' = helpBS env args in
        DFun (name, newArgs, helpE funenv env' body, tag)
  and helpB env b =
    match b with
    | BBlank _ -> (b, env)
    | BName (name, allow_shadow, tag) ->
        let name' = sprintf "%s_%d" name tag in
        (BName (name', allow_shadow, tag), (name, name') :: env)
    | BTuple (binds, tag) ->
        let binds', env' = helpBS env binds in
        (BTuple (binds', tag), env')
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> ([], env)
    | b :: bs ->
        let b', env' = helpB env b in
        let bs', env'' = helpBS env' bs in
        (b' :: bs', env'')
  and helpBG funenv env (bindings : tag binding list) =
    match bindings with
    | [] -> ([], env)
    | (b, e, a) :: bindings ->
        let b', env' = helpB env b in
        let e' = helpE funenv env e in
        let bindings', env'' = helpBG funenv env' bindings in
        ((b', e', a) :: bindings', env'')
  and helpE funenv env e =
    match e with
    | ESeq (e1, e2, tag) -> ESeq (helpE funenv env e1, helpE funenv env e2, tag)
    | ETuple (es, tag) -> ETuple (List.map (helpE funenv env) es, tag)
    | EGetItem (e, idx, tag) -> EGetItem (helpE funenv env e, helpE funenv env idx, tag)
    | ESetItem (e, idx, newval, tag) ->
        ESetItem (helpE funenv env e, helpE funenv env idx, helpE funenv env newval, tag)
    | EPrim1 (op, arg, tag) -> EPrim1 (op, helpE funenv env arg, tag)
    | EPrim2 (op, left, right, tag) ->
        EPrim2 (op, helpE funenv env left, helpE funenv env right, tag)
    | EIf (c, t, f, tag) -> EIf (helpE funenv env c, helpE funenv env t, helpE funenv env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EId (name, tag) -> ( try EId (find env name, tag) with Not_found -> e )
    | EApp (name, args, native, tag) ->
        let call_type =
          match find_opt funenv name with
          | None -> native
          | Some ct -> ct
        in
        EApp (name, List.map (helpE funenv env) args, call_type, tag)
    | ELet (binds, body, tag) ->
        let binds', env' = helpBG funenv env binds in
        let body' = helpE funenv env' body in
        ELet (binds', body', tag)
  in
  rename [] p
;;

(* Returns the stack-index (in words) of the deepest stack index used for any
   of the variables in this expression *)
let deepest_stack e env =
  let rec helpA e =
    match e with
    | ALet (name, bind, body, _) ->
        List.fold_left max 0 [name_to_offset name; helpC bind; helpA body]
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf (c, t, f, _) -> List.fold_left max 0 [helpI c; helpA t; helpA f]
    | CPrim1 (_, i, _) -> helpI i
    | CPrim2 (_, i1, i2, _) -> max (helpI i1) (helpI i2)
    | CApp (_, args, _, _) -> List.fold_left max 0 (List.map helpI args)
    | CTuple (elms, _) -> List.fold_left max 0 (List.map helpI elms)
    | CGetItem (tup, idx, _) -> max (helpI tup) (helpI idx)
    | CSetItem (tup, idx, newval, _) -> List.fold_left max 0 [helpI tup; helpI idx; helpI newval]
    | CImmExpr i -> helpI i
  and helpI i =
    match i with
    | ImmNum _ -> 0
    | ImmBool _ -> 0
    | ImmNil _ -> 0
    | ImmId (name, _) -> name_to_offset name
  and name_to_offset name =
    match find env name with
    | RegOffset (bytes, RBP) -> bytes / (-1 * word_size) (* negative because stack direction *)
    | _ -> 0
  in
  max (helpA e) 0 (* if only parameters are used, helpA might return a negative value *)
;;

(* IMPLEMENT EVERYTHING BELOW *)

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program (decls, body, _) -> AProgram (List.map helpD decls, helpA body, ())
  and helpD (d : tag decl) : unit adecl =
    match d with
    | DFun (name, args, body, _) ->
        let args =
          List.map
            (fun a ->
              match a with
              | BName (a, _, _) -> a
              | _ ->
                  raise (InternalCompilerError "Blank and tuples should be desugared away in args")
              )
            args
        in
        ADFun (name, args, helpA body, ())
  and helpC (e : tag expr) : unit cexpr * (string * unit cexpr) list =
    match e with
    | EPrim1 (op, arg, _) ->
        let arg_imm, arg_setup = helpI arg in
        (CPrim1 (op, arg_imm, ()), arg_setup)
    | EPrim2 (op, left, right, _) ->
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        (CPrim2 (op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf (cond, _then, _else, _) ->
        let cond_imm, cond_setup = helpI cond in
        (CIf (cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet ([], body, _) -> helpC body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
        let _, exp_setup = helpI exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ body_setup)
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
        raise (InternalCompilerError "ELet Tuples should be dusugared away")
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EApp (funname, args, ct, _) ->
        let new_args, new_setup = List.split (List.map helpI args) in
        (CApp (funname, new_args, ct, ()), List.concat new_setup)
    | ETuple (tups, _) ->
        let tup_imms, tups_setup =
          List.fold_right
            (fun e (imms, setups) ->
              let imm, setup = helpI e in
              (imm :: imms, setup @ setups) )
            tups ([], [])
        in
        (CTuple (tup_imms, ()), tups_setup)
    | EGetItem (tup, index, _) ->
        let tup_imm, tup_setup = helpI tup in
        let index_imm, index_setup = helpI index in
        (CGetItem (tup_imm, index_imm, ()), tup_setup @ index_setup)
    | ESetItem (tup, index, new_val, _) ->
        let tup_imm, tup_setup = helpI tup in
        let index_imm, index_setup = helpI index in
        let new_val_imm, new_val_setup = helpI new_val in
        (CSetItem (tup_imm, index_imm, new_val_imm, ()), tup_setup @ index_setup @ new_val_setup)
    | _ ->
        let imm, setup = helpI e in
        (CImmExpr imm, setup)
  and helpI (e : tag expr) : unit immexpr * (string * unit cexpr) list =
    match e with
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (name, _) -> (ImmId (name, ()), [])
    | ENil _ -> (ImmNil (), [])
    | EPrim1 (op, arg, tag) ->
        let tmp = sprintf "unary_%d" tag in
        let arg_imm, arg_setup = helpI arg in
        (ImmId (tmp, ()), arg_setup @ [(tmp, CPrim1 (op, arg_imm, ()))])
    | EPrim2 (op, left, right, tag) ->
        let tmp = sprintf "binop_%d" tag in
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        (ImmId (tmp, ()), left_setup @ right_setup @ [(tmp, CPrim2 (op, left_imm, right_imm, ()))])
    | EIf (cond, _then, _else, tag) ->
        let tmp = sprintf "if_%d" tag in
        let cond_imm, cond_setup = helpI cond in
        (ImmId (tmp, ()), cond_setup @ [(tmp, CIf (cond_imm, helpA _then, helpA _else, ()))])
    | EApp (funname, args, ct, tag) ->
        let tmp = sprintf "app_%d" tag in
        let new_args, new_setup = List.split (List.map helpI args) in
        (ImmId (tmp, ()), List.concat new_setup @ [(tmp, CApp (funname, new_args, ct, ()))])
    | ELet ([], body, _) -> helpI body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
        let _, exp_setup = helpI exp in
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ body_setup)
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
        raise (InternalCompilerError "ELet Tuples should be dusugared away")
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | ESeq (_, _, _) -> raise (InternalCompilerError "Sequences should be desugared away")
    | ETuple (tups, tag) ->
        let tup_imms, tups_setup =
          List.fold_right
            (fun e (imms, setups) ->
              let imm, setup = helpI e in
              (imm :: imms, setup @ setups) )
            tups ([], [])
        in
        let tmp = sprintf "tuple_%d" tag in
        (ImmId (tmp, ()), tups_setup @ [(tmp, CTuple (tup_imms, ()))])
    | EGetItem (tup, index, tag) ->
        let tup_imm, tup_setup = helpI tup in
        let index_imm, index_setup = helpI index in
        let tmp = sprintf "getitem_%d" tag in
        (ImmId (tmp, ()), tup_setup @ index_setup @ [(tmp, CGetItem (tup_imm, index_imm, ()))])
    | ESetItem (tup, index, new_val, tag) ->
        let tup_imm, tup_setup = helpI tup in
        let index_imm, index_setup = helpI index in
        let new_val_imm, new_val_setup = helpI new_val in
        let tmp = sprintf "setitem_%d" tag in
        ( ImmId (tmp, ()),
          tup_setup @ index_setup @ new_val_setup
          @ [(tmp, CSetItem (tup_imm, index_imm, new_val_imm, ()))] )
  and helpA e : unit aexpr =
    let ans, ans_setup = helpC e in
    List.fold_right (fun (bind, exp) body -> ALet (bind, exp, body, ())) ans_setup (ACExpr ans)
  in
  helpP p
;;

let is_well_formed (p : sourcespan program) : sourcespan program fallible =
  let rec wf_E
      (bindings : (string * sourcespan) list)
      (functions : (string * (sourcespan * int)) list)
      (e : sourcespan expr) : exn list =
    match e with
    | ENumber (n, s) ->
        if n > Int64.div Int64.max_int 2L || n < Int64.div Int64.min_int 2L then
          [Overflow (n, s)]
        else
          []
    | EBool (_, _) -> []
    | EId (x, s) -> (
      try
        ignore (List.assoc x bindings);
        []
      with Not_found -> [UnboundId (x, s)] )
    | EApp (funname, args, _, s) -> (
    let arg_exn_list = List.concat_map (wf_E bindings functions) args in
      try
        let _, arity = List.assoc funname functions in
        let arg_length = List.length args in
        if arity = arg_length then
          arg_exn_list
        else
          [Arity (arity, arg_length, s)] @ arg_exn_list
      with Not_found -> 
        (try let _ = (List.assoc funname initial_fun_env) in arg_exn_list
      with Not_found -> [UnboundFun (funname, s)] @ arg_exn_list))
    | ELet (tree_binds, body, _) ->
        let binds =
          List.fold_right
            (fun (bind, exp, _) so_far ->
              match bind with
              | BBlank _ -> so_far
              | BName (str, _, loc) -> (str, exp, loc) :: so_far
              | BTuple (bind_li, _) -> flatten_binds bind_li exp @ so_far )
            tree_binds []
        in
        let exn2, env2, _ =
          List.fold_left
            (fun (exn_list, scope_env, shadow_env) (x, e, loc) ->
              try
                let existing = List.assoc x shadow_env in
                ( wf_E scope_env functions e @ [DuplicateId (x, loc, existing)] @ exn_list,
                  scope_env,
                  shadow_env )
              with Not_found ->
                ( wf_E scope_env functions e @ exn_list,
                  (x, loc) :: scope_env,
                  (x, loc) :: shadow_env ) )
            ([], bindings, []) binds
        in
        exn2 @ wf_E env2 functions body
    | EPrim1 (_, e, _) -> wf_E bindings functions e
    | EPrim2 (_, l, r, _) -> wf_E bindings functions l @ wf_E bindings functions r
    | EIf (cond, thn, els, _) ->
        let function_app = wf_E bindings functions in
        function_app cond @ function_app thn @ function_app els
    | ETuple (exprs, _) -> List.concat (List.map (wf_E bindings functions) exprs)
    | EGetItem (tup, index, _) ->
        let tup_wf = wf_E bindings functions tup in
        let index_wf = wf_E bindings functions index in
        tup_wf @ index_wf
    | ESetItem (tup, index, new_expr, _) ->
        let tup_wf = wf_E bindings functions tup in
        let index_wf = wf_E bindings functions index in
        let expr_wf = wf_E bindings functions new_expr in
        tup_wf @ index_wf @ expr_wf
    | ESeq (fst, snd, _) ->
        let fst_wf = wf_E bindings functions fst in
        let snd_wf = wf_E bindings functions snd in
        fst_wf @ snd_wf
    | ENil _ -> []
  and flatten_binds (binds : sourcespan bind list) (exp : sourcespan expr) :
      (string * sourcespan expr * sourcespan) list =
    match binds with
    | BBlank _ :: rest -> flatten_binds rest exp
    | BName (str, _, loc) :: rest -> (str, exp, loc) :: flatten_binds rest exp
    | BTuple (bind_li, _) :: rest -> flatten_binds bind_li exp @ flatten_binds rest exp
    | [] -> []
  and wf_D (functions : (string * (sourcespan * int)) list) (d : sourcespan decl) :
      exn list * (string * (sourcespan * int)) list =
    match d with
    | DFun (funname, old_args, body, source) -> (
        let args = flatten_args old_args in
        let exn_list, new_args =
          List.fold_left
            (fun (exn_list, new_args) (x, loc) ->
              try
                let existing = List.assoc x new_args in
                (DuplicateId (x, loc, existing) :: exn_list, new_args)
              with Not_found -> (exn_list, (x, loc) :: new_args) )
            ([], []) args
        in
        try
          let old_source, _ = List.assoc funname functions in
          ( exn_list @ (DuplicateFun (funname, source, old_source) :: wf_E args functions body),
            functions )
        with Not_found ->
          let new_functions = (funname, (source, List.length old_args)) :: functions in
          (exn_list @ wf_E args new_functions body, new_functions) )
  and flatten_args (binds : sourcespan bind list) : (string * sourcespan) list =
    match binds with
    | BBlank _ :: rest -> flatten_args rest
    | BName (str, _, loc) :: rest -> (str, loc) :: flatten_args rest
    | BTuple (bind_li, _) :: rest -> flatten_args bind_li @ flatten_args rest
    | [] -> []
  in
  match p with
  | Program (decls, body, _) ->
      let decl_exn_list, functions =
        List.fold_left
          (fun (decl_exn_list, functions) decl ->
            let exn_list, decl_funs = wf_D functions decl in
            (exn_list @ decl_exn_list, decl_funs) )
          ([], []) decls
      in
      let body_exn_list = wf_E [] functions body in
      let all_exn = decl_exn_list @ body_exn_list in
      if List.length all_exn = 0 then
        Ok p
      else
        Error all_exn
;;

let desugar (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    fun name ->
      next := !next + 1;
      sprintf "%s_%d" name !next
  in
  let rec help (e : sourcespan expr) =
    match e with
    | EId (x, s) -> EId (x, s)
    | EBool (x, s) -> EBool (x, s)
    | ENil s -> ENil s
    | ELet (x, b, s) ->
        let new_binds =
          List.fold_right
            (fun (bind, exp, s) so_far ->
              match bind with
              | BBlank _ -> (bind, help exp, s) :: so_far
              | BName (_, _, _) -> (bind, help exp, s) :: so_far
              | BTuple (bind_li, s) ->
                  let tmp = gensym "let_tuple" in
                  let new_id = EId (tmp, s) in
                  let new_binding = (BName (tmp, false, s), help exp, s) in
                  [new_binding] @ tuple_helper bind_li new_id @ so_far )
            x []
        in
        ELet (new_binds, help b, s)
    | ENumber (n, s) -> ENumber (n, s)
    | EPrim1 (op, e, s) -> 
      (match op with
      | Print -> EApp("print", [help e], Native, s)
      | _ -> EPrim1 (op, help e, s))
    | EPrim2 (And, e1, e2, s) ->
        EIf
          (EPrim1 (Not, help e1, s), EBool (false, s), EPrim1 (Not, EPrim1 (Not, help e2, s), s), s)
    | EPrim2 (Or, e1, e2, s) ->
        EIf (EPrim1 (Not, help e1, s), EPrim1 (Not, EPrim1 (Not, help e2, s), s), EBool (true, s), s)
    | EPrim2 (op, e1, e2, s) -> EPrim2 (op, help e1, help e2, s)
    | EIf (cond, thn, els, s) -> EIf (help cond, help thn, help els, s)
    | EApp (funname, args, call_t, s) -> EApp (funname, List.map help args, call_t, s)
    | ESeq (e1, e2, s) -> ELet ([(BBlank s, help e1, s)], help e2, s)
    | ETuple (es, s) -> ETuple (List.map help es, s)
    | EGetItem (e, idx, s) -> EGetItem (help e, help idx, s)
    | ESetItem (e, idx, newval, s) -> ESetItem (help e, help idx, help newval, s)
  and tuple_helper (bind_li : sourcespan bind list) (exp : sourcespan expr) :
      sourcespan binding list =
    List.concat
      (List.mapi
         (fun i bind ->
           match bind with
           | BTuple (li, s) -> tuple_helper li (EGetItem (exp, ENumber (Int64.of_int i, s), s))
           | BBlank s -> [(bind, EGetItem (exp, ENumber (Int64.of_int i, s), s), s)]
           | BName (_, _, s) -> [(bind, EGetItem (exp, ENumber (Int64.of_int i, s), s), s)] )
         bind_li )
  in
  let rec helpD (d : sourcespan decl) : sourcespan decl =
    match d with
    | DFun (name, args, body, s) ->
        let new_args, let_binds =
          List.fold_right
            (fun arg (args, let_binds) ->
              match arg with
              | BBlank _ -> (arg :: args, let_binds)
              | BName (_, _, _) -> (arg :: args, let_binds)
              | BTuple (bind_li, s) ->
                  let tmp = gensym "let_tuple" in
                  let new_id = EId (tmp, s) in
                  let new_arg = BName (tmp, false, s) in
                  (new_arg :: args, tuple_helper bind_li new_id @ let_binds) )
            args ([], [])
        in
        if List.length let_binds = 0 then
          DFun (name, new_args, help body, s)
        else
          let new_body = ELet (let_binds, help body, s) in
          DFun (name, new_args, new_body, s)
  in
  let helpP (p : sourcespan program) =
    match p with
    | Program (decls, e, tag) -> Program (List.map helpD decls, help e, tag)
  in
  helpP p
;;

let naive_stack_allocation (prog : tag aprogram) : tag aprogram * arg envt =
  let rec helpAExpr (si : int) (e : tag aexpr) : arg envt =
    match e with
    | ALet (str, cexp, aexp, _) ->
        let new_arg = [(str, RegOffset (~-8 * si, RBP))] in
        new_arg @ helpCExpr (si + 1) cexp @ helpAExpr (si + 1) aexp
    | ACExpr cexp -> helpCExpr si cexp
  and helpCExpr (si : int) (e : tag cexpr) : arg envt =
    match e with
    | CIf (_, thn_aexp, els_aexp, _) -> helpAExpr si thn_aexp @ helpAExpr si els_aexp
    | _ -> []
  and helpD (d : tag adecl) : arg envt =
    match d with
    | ADFun (_, args, body, _) ->
        let arg_locs = List.mapi (fun i arg -> (arg, RegOffset ((8 * i) + 16, RBP))) args in
        arg_locs @ helpAExpr 1 body
  in
  match prog with
  | AProgram (decls, body, _) ->
      let decl_env = List.concat (List.map helpD decls) in
      let body_env = helpAExpr 1 body in
      (prog, decl_env @ body_env)
;;

(* All are using RAX right now, just in case any changes need to be made *)
let bool_op_prologue reg reg_arg =
  [ IMov (Reg reg, reg_arg);
    IAnd (Reg reg, HexConst bool_tag_mask);
    ICmp (Reg reg, HexConst bool_tag);
    IMov (Reg reg, reg_arg) ]
;;

let tuple_op_prologue reg reg_arg =
  [ IMov (Reg reg, reg_arg);
    IAnd (Reg reg, HexConst bool_tag_mask);
    ICmp (Reg reg, HexConst pair_tag);
    IMov (Reg reg, reg_arg) ]
;;

let number_op_prologue reg reg_arg =
  [ IMov (Reg reg, reg_arg);
    IAnd (Reg reg, HexConst num_tag_mask);
    ICmp (Reg reg, HexConst num_tag);
    IMov (Reg reg, reg_arg) ]
;;

let rec compile_fun (fun_name : string) (args : string list) (env : arg envt) (e : tag aexpr) :
    instruction list =
  let rounded_stack_number =
    let num_vars = deepest_stack e env in
    if num_vars mod 2 = 0 then
      num_vars
    else
      num_vars + 1
  in
  let rec pushes i =
    if i = rounded_stack_number then
      []
    else
      IPush (Const 0L) :: pushes (i + 1)
  in
  [ILabel fun_name; IPush (Reg RBP); IMov (Reg RBP, Reg RSP)]
  @ pushes 0
  @ [ILabel (fun_name ^ "_body")]
  @ [IMov (Reg RSP, Reg RBP); ISub (Reg RSP, Const (Int64.of_int (8 * rounded_stack_number)))]

and compile_aexpr (e : tag aexpr) (env : arg envt) (num_args : int) (is_tail : bool) :
    instruction list =
  match e with
  | ALet (x, exp, body, _) ->
      let prelude = compile_cexpr exp env num_args false in
      let body = compile_aexpr body env num_args is_tail in
      prelude @ [IMov (find env x, Reg RAX)] @ body
  | ACExpr cexp -> compile_cexpr cexp env num_args is_tail

and compile_cexpr (e : tag cexpr) (env : arg envt) (num_args : int) (is_tail : bool) :
    instruction list =
  match e with
  | CIf (cond, thn, els, tag) ->
      let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
      [ IMov (Reg RAX, compile_imm cond env);
        IAnd (Reg RAX, HexConst bool_tag_mask);
        IMov (Reg scratch_reg, HexConst bool_tag);
        ICmp (Reg RAX, Reg scratch_reg);
        IJne "err_if_not_bool";
        IMov (Reg RAX, compile_imm cond env);
        IMov (Reg scratch_reg, const_false);
        ICmp (Reg RAX, Reg scratch_reg);
        IJe else_label ]
      @ compile_aexpr thn env num_args is_tail
      @ [IJmp done_label; ILabel else_label]
      @ compile_aexpr els env num_args is_tail
      @ [ILabel done_label]
  | CApp (funname, args, call_t, _) -> (
    try
      let _ = List.assoc funname initial_fun_env in
      let push_args_instr =
        List.concat
          (List.rev
             (List.mapi
                (fun i imm ->
                  if i < 6 then
                    [IMov (Reg (List.nth first_six_args_registers i), compile_imm imm env)]
                  else
                    [IMov (Reg R10, compile_imm imm env); IPush (Reg R10)] )
                args ) )
      in
      let pop_args_instr =
        List.concat
          (List.mapi
             (fun i _ ->
               if i < 6 then
                 []
               else
                 [IPop (Reg R10)] )
             args )
      in
      push_args_instr @ [ICall funname] @ pop_args_instr
    with Not_found ->
      let args =
        if List.length args mod 2 = 1 then
          args @ [ImmNum (0L, 0)]
        else
          args
      in
      let push_args_instr =
        List.concat
          (List.rev
             (List.mapi (fun _ imm -> [IMov (Reg R10, compile_imm imm env); IPush (Reg R10)]) args) )
      in
      let pop_args_instr = List.map (fun _ -> IPop (Reg R10)) args in
      let push_swap_instr =
        List.concat
          (List.rev
             (List.mapi (fun _ arg -> [IMov (Reg RAX, compile_imm arg env); IPush (Reg RAX)]) args) )
      in
      let pop_swap_instr =
        List.concat
          (List.mapi
             (fun i _ -> [IPop (Reg RAX); IMov (RegOffset (16 + (i * 8), RBP), Reg RAX)])
             args )
      in
      if true && is_tail && List.length args <= num_args then
        push_swap_instr @ pop_swap_instr @ [IJmp (funname ^ "_body")]
      else
        push_args_instr @ [ICall funname] @ pop_args_instr )
  | CImmExpr e -> [IMov (Reg RAX, compile_imm e env)]
  | CPrim1 (Add1, arg, _) ->
      number_op_prologue RAX (compile_imm arg env)
      @ [IJne "err_arith_not_num"; IAdd (Reg RAX, Const 2L); IJo "err_overflow"]
  | CPrim1 (Sub1, arg, _) ->
      number_op_prologue RAX (compile_imm arg env)
      @ [IJne "err_arith_not_num"; ISub (Reg RAX, Const 2L); IJo "err_overflow"]
  | CPrim1 (Print, arg, _) ->
      [IMov (Reg RAX, compile_imm arg env); IMov (Reg RDI, Reg RAX); ICall "print"]
  | CPrim1 (IsBool, arg, tag) ->
      let label_tag = sprintf "is_bool_true#%d" tag in
      bool_op_prologue RAX (compile_imm arg env)
      @ [IMov (Reg RAX, const_true); IJe label_tag; IMov (Reg RAX, const_false); ILabel label_tag]
  | CPrim1 (IsNum, arg, tag) ->
      let label_tag = sprintf "is_num_true#%d" tag in
      number_op_prologue RAX (compile_imm arg env)
      @ [IMov (Reg RAX, const_true); IJe label_tag; IMov (Reg RAX, const_false); ILabel label_tag]
  | CPrim1 (IsTuple, arg, tag) ->
      let label_tag = sprintf "is_tuple_true#%d" tag in
      tuple_op_prologue RAX (compile_imm arg env)
      @ [IMov (Reg RAX, const_true); IJe label_tag; IMov (Reg RAX, const_false); ILabel label_tag]
  | CPrim1 (Not, arg, _) ->
      bool_op_prologue RAX (compile_imm arg env)
      @ [ IJne "err_logic_not_bool";
          IMov (Reg scratch_reg, bool_mask);
          IXor (Reg RAX, Reg scratch_reg) ]
  | CPrim1 (PrintStack, arg, _) ->
      [ (* Not fully implemented yet *)
        IMov (Reg RAX, compile_imm arg env);
        IMov (Reg RDI, Reg RAX);
        IMov (Reg RSI, Const (Int64.of_int num_args));
        ICall "printstack" ]
  | CPrim2 (Plus, l, r, _) ->
      let r_arg = compile_imm r env in
      number_op_prologue RAX r_arg @ [IJne "err_arith_not_num"]
      @ number_op_prologue RAX (compile_imm l env)
      @ [ IJne "err_arith_not_num";
          IMov (Reg scratch_reg, r_arg);
          IAdd (Reg RAX, Reg scratch_reg);
          IJo "err_overflow" ]
  | CPrim2 (Minus, l, r, _) ->
      let r_arg = compile_imm r env in
      number_op_prologue RAX r_arg @ [IJne "err_arith_not_num"]
      @ number_op_prologue RAX (compile_imm l env)
      @ [ IJne "err_arith_not_num";
          IMov (Reg scratch_reg, r_arg);
          ISub (Reg RAX, Reg scratch_reg);
          IJo "err_overflow" ]
  | CPrim2 (Times, l, r, _) ->
      let r_arg = compile_imm r env in
      number_op_prologue RAX r_arg @ [IJne "err_arith_not_num"]
      @ number_op_prologue RAX (compile_imm l env)
      @ [ IJne "err_arith_not_num";
          IMov (Reg scratch_reg, r_arg);
          ISar (Reg RAX, Const 1L);
          IMul (Reg RAX, Reg scratch_reg);
          IJo "err_overflow" ]
  | CPrim2 (And, l, r, _) ->
      let r_arg = compile_imm r env in
      bool_op_prologue RAX r_arg @ [IJne "err_logic_not_bool"]
      @ bool_op_prologue RAX (compile_imm l env)
      @ [IJne "err_logic_not_bool"; IMov (Reg scratch_reg, r_arg); IAnd (Reg RAX, Reg scratch_reg)]
  | CPrim2 (Or, l, r, _) ->
      let r_arg = compile_imm r env in
      bool_op_prologue RAX r_arg @ [IJne "err_logic_not_bool"]
      @ bool_op_prologue RAX (compile_imm l env)
      @ [IJne "err_logic_not_bool"; IMov (Reg scratch_reg, r_arg); IOr (Reg RAX, Reg scratch_reg)]
  | CPrim2 (Greater, l, r, tag) ->
      let jump_label = sprintf "greater#%d" tag in
      let r_arg = compile_imm r env in
      number_op_prologue RAX r_arg @ [IJne "err_comp_not_num"]
      @ number_op_prologue RAX (compile_imm l env)
      @ [ IJne "err_comp_not_num";
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJg jump_label;
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (GreaterEq, l, r, tag) ->
      let r_arg = compile_imm r env in
      let jump_label = sprintf "greater_eq#%d" tag in
      number_op_prologue RAX r_arg @ [IJne "err_comp_not_num"]
      @ number_op_prologue RAX (compile_imm l env)
      @ [ IJne "err_comp_not_num";
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJge jump_label;
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (Less, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "less#%d" tag in
      number_op_prologue RAX r_arg @ [IJne "err_comp_not_num"] @ number_op_prologue RAX l_arg
      @ [ IJne "err_comp_not_num";
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJl jump_label;
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (LessEq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "less_eq#%d" tag in
      number_op_prologue RAX r_arg @ [IJne "err_comp_not_num"] @ number_op_prologue RAX l_arg
      @ [ IJne "err_comp_not_num";
          IMov (Reg scratch_reg, r_arg);
          ICmp (Reg RAX, Reg scratch_reg);
          IMov (Reg RAX, const_true);
          IJle jump_label;
          IMov (Reg RAX, const_false);
          ILabel jump_label ]
  | CPrim2 (Eq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "eq#%d" tag in
      [ IMov (Reg RAX, l_arg);
        IMov (Reg scratch_reg, r_arg);
        ICmp (Reg RAX, Reg scratch_reg);
        IMov (Reg RAX, const_true);
        IJe jump_label;
        IMov (Reg RAX, const_false);
        ILabel jump_label ]
  | CTuple (immexpr_li, tag) ->
      let tup_size = List.length immexpr_li in
      let alloc_size_wrds =
        if (tup_size + 1) mod 2 = 0 then
          tup_size + 1
        else
          tup_size + 2
      in
      let mov_size_instr =
        [ IMov (Reg scratch_reg, compile_imm (ImmNum (Int64.of_int tup_size, tag)) env);
          IMov (RegOffset (0, heap_reg), Reg scratch_reg) ]
      in
      let mov_instrs =
        List.concat
          (List.mapi
             (fun i imm ->
               [ IMov (Reg scratch_reg, compile_imm imm env);
                 IMov (RegOffset (8 * (i + 1), heap_reg), Reg scratch_reg) ] )
             immexpr_li )
      in
      let bump_heap_instrs =
        [ IMov (Reg RAX, Reg heap_reg);
          IAdd (Reg RAX, HexConst pair_tag);
          IAdd (Reg heap_reg, Const (Int64.of_int (8 * alloc_size_wrds))) ]
      in
      mov_size_instr @ mov_instrs @ bump_heap_instrs
  | CSetItem (tup, index, new_val, _) ->
      let tup_c = compile_imm tup env in
      let index_c = compile_imm index env in
      let new_val_c = compile_imm new_val env in
      let tup_prologue = tuple_op_prologue RAX tup_c in
      let index_prologue = number_op_prologue scratch_reg index_c in
      let check_tuple_and_check_nil = [
        IJne("err_get_not_tuple");
        IMov (Reg RAX, tup_c);
        ICmp (Reg RAX, nil_val);
        IJe("err_nil_deref");
      ]
      in
      let check_index = [
        IJne("err_index_not_num")
      ]
      in
      let untag_and_cmp =
        [ ISub (Reg RAX, HexConst pair_tag);
          ICmp (Reg scratch_reg, Const 0L);
          IJl "err_get_low_index";
          IMov (Reg RAX, RegOffset (0, RAX));
          ICmp (Reg scratch_reg, Reg RAX);
          IJge "err_get_high_index" ]
      in
      let set_instr =
        [ IMov (Reg RAX, tup_c);
          ISub (Reg RAX, HexConst pair_tag);
          IMov (Reg scratch_reg, index_c);
          ISar (Reg scratch_reg, Const 1L);
          IAdd (Reg scratch_reg, Const 1L);
          IMul (Reg scratch_reg, Const (Int64.of_int word_size));
          IAdd (Reg scratch_reg, Reg RAX);
          IMov (Reg RAX, new_val_c);
          IMov (RegOffset(0, scratch_reg), Reg RAX);
          IMov (Reg RAX, nil_val) ]
      in
      tup_prologue @ check_tuple_and_check_nil @ index_prologue @ check_index @ untag_and_cmp @ set_instr
  | CGetItem (tup, index, _) ->
      let tup_c = compile_imm tup env in
      let index_c = compile_imm index env in
      let tup_prologue = tuple_op_prologue RAX tup_c in
      let index_prologue = number_op_prologue scratch_reg index_c in
      let check_tuple_and_check_nil = [
        IJne("err_get_not_tuple");
        IMov (Reg RAX, tup_c);
        ICmp (Reg RAX, nil_val);
        IJe("err_nil_deref");
      ]
      in
      let check_index = [
        IJne("err_index_not_num")
      ]
      in
      let untag_and_cmp =
        [ ISub (Reg RAX, HexConst pair_tag);
          ICmp (Reg scratch_reg, Const 0L);
          IJl "err_get_low_index";
          IMov (Reg RAX, RegOffset (0, RAX));
          ICmp (Reg scratch_reg, Reg RAX);
          IJge "err_get_high_index" ]
      in
      let set_instr =
        [ IMov (Reg RAX, tup_c);
          ISub (Reg RAX, HexConst pair_tag);
          IMov (Reg scratch_reg, index_c);
          ISar (Reg scratch_reg, Const 1L);
          IMov (Reg RAX, RegOffsetReg (RAX, scratch_reg, word_size, 8)) ]
      in
      tup_prologue @ check_tuple_and_check_nil @ index_prologue @ check_index @ untag_and_cmp @ set_instr

and compile_imm (e : tag immexpr) (env : arg envt) =
  match e with
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find env x
  | ImmNil _ -> nil_val
;;

let compile_decl (env : arg envt) (d : tag adecl) : instruction list =
  match d with
  | ADFun (funname, args, aexp, _) ->
      let compiled_fun = compile_fun funname args env aexp in
      let postlude = [IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet] in
      compiled_fun @ compile_aexpr aexp env (List.length args) true @ postlude
;;

let compile_prog ((anfed : tag aprogram), (env : arg envt)) : string =
  match anfed with
  | AProgram (decls, body, _) ->
      let prelude =
        "section .text\n\
         extern error\n\
         extern print\n\
         extern printstack\n\
         extern set_stack_bottom\n\
         extern input\n\
         extern equal\n\
         global our_code_starts_here"
      in
      let call_stack_bottom =
        [IMov (Reg RDI, Reg RBP); IMov (Reg RSI, Reg RAX); ICall "set_stack_bottom"]
      in
      let comp_decls = List.concat (List.map (compile_decl env) decls) in
      let body_prologue, comp_body, body_epilogue =
        let rounded_stack_number =
          let num_vars = deepest_stack body env in
          if num_vars mod 2 = 0 then
            num_vars
          else
            num_vars + 1
        in
        let rec pushes i =
          if i = rounded_stack_number then
            []
          else
            IPush (Const 0L) :: pushes (i + 1)
        in
        ( [ILabel "our_code_starts_here"; IPush (Reg RBP); IMov (Reg RBP, Reg RSP)] @ pushes 0,
          compile_aexpr body env 0 true,
          [IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet] )
      in
      let heap_start =
        [ ILineComment "heap start";
          IInstrComment
            ( IMov (Reg heap_reg, Reg (List.nth first_six_args_registers 0)),
              "Load heap_reg with our argument, the heap pointer" );
          IInstrComment (IAdd (Reg heap_reg, Const 15L), "Align it to the nearest multiple of 16");
          IMov (Reg scratch_reg, HexConst 0xFFFFFFFFFFFFFFF0L);
          IInstrComment (IAnd (Reg heap_reg, Reg scratch_reg), "by adding no more than 15 to it") ]
      in
      let error_functions =
        [ ILineComment "Start of error functions";
          ILabel "err_arith_not_num";
          IMov (Reg RDI, Const err_ARITH_NOT_NUM);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_comp_not_num";
          IMov (Reg RDI, Const err_COMP_NOT_NUM);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_logic_not_bool";
          IMov (Reg RDI, Const err_LOGIC_NOT_BOOL);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_if_not_bool";
          IMov (Reg RDI, Const err_IF_NOT_BOOL);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_get_not_tuple";
          IMov (Reg RDI, Const err_GET_NOT_TUPLE);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_get_low_index";
          IMov (Reg RDI, Const err_GET_LOW_INDEX);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_get_high_index";
          IMov (Reg RDI, Const err_GET_HIGH_INDEX);
          IMov (Reg RSI, Reg scratch_reg);
          ICall "error";
          ILabel "err_nil_deref";
          IMov (Reg RDI, Const err_NIL_DEREF);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_overflow";
          IMov (Reg RDI, Const err_OVERFLOW);
          IMov (Reg RSI, Reg RAX);
          ICall "error";
          ILabel "err_index_not_num";
          IMov (Reg RDI, Const err_INDEX_NOT_NUMBER);
          IMov (Reg RSI, Reg RAX);
          ICall "error"
          ]
      in
      let main =
        to_asm
          ( comp_decls @ body_prologue @ heap_start @ call_stack_bottom @ comp_body @ body_epilogue
          @ error_functions )
      in
      sprintf "%s%s\n" prelude main
;;

(* Feel free to add additional phases to your pipeline.
   The final pipeline phase needs to return a string,
   but everything else is up to you. *)

let run_if should_run f =
  if should_run then
    f
  else
    no_op_phase
;;

(* Add a desugaring phase somewhere in here *)
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_err_phase well_formed is_well_formed
  |> add_phase desugared desugar |> add_phase tagged tag |> add_phase renamed rename_and_tag
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase locate_bindings naive_stack_allocation
  |> add_phase result compile_prog
;;
