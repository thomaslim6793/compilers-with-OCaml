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

let bool_tag = 0x0000000000000001L

let bool_tag_mask = 0x0000000000000007L

let num_tag = 0x0000000000000000L

let num_tag_mask = 0x0000000000000001L

let err_COMP_NOT_NUM = 1L

let err_ARITH_NOT_NUM = 2L

let err_LOGIC_NOT_BOOL = 3L

let err_IF_NOT_BOOL = 4L

let err_OVERFLOW = 5L

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]

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
    | CApp (_, args, _) -> List.fold_left max 0 (List.map helpI args)
    | CImmExpr i -> helpI i
  and helpI i =
    match i with
    | ImmNum _ -> 0
    | ImmBool _ -> 0
    | ImmId (name, _) -> name_to_offset name
  and name_to_offset name =
    match find env name with
    | RegOffset (bytes, RBP) -> bytes / (-1 * word_size) (* negative because stack direction *)
    | _ -> 0
  in
  max (helpA e) 0 (* if only parameters are used, helpA might return a negative value *)
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

(* IMPLEMENT EVERYTHING BELOW *)

let rec short_circuit_desugar (e : sourcespan program) : sourcespan program =
  let rec help (e : sourcespan expr) =
    match e with
    | EId (x, s) -> EId (x, s)
    | EBool (x, s) -> EBool (x, s)
    | ELet (x, b, s) -> ELet (x, b, s)
    | ENumber (n, s) -> ENumber (n, s)
    | EPrim1 (op, e, s) -> EPrim1 (op, help e, s)
    | EPrim2 (And, e1, e2, s) ->
        EIf
          (EPrim1 (Not, help e1, s), EBool (false, s), EPrim1 (Not, EPrim1 (Not, help e2, s), s), s)
    | EPrim2 (Or, e1, e2, s) ->
        EIf (EPrim1 (Not, help e1, s), EPrim1 (Not, EPrim1 (Not, help e2, s), s), EBool (true, s), s)
    | EPrim2 (op, e1, e2, s) -> EPrim2 (op, help e1, help e2, s)
    | EIf (cond, thn, els, s) -> EIf (help cond, help thn, help els, s)
    | EApp (funname, args, s) -> EApp (funname, List.map help args, s)
  in
  let helpD (d : sourcespan decl) : sourcespan decl =
    match d with
    | DFun (name, args, body, tag) -> DFun (name, args, help body, tag)
  in
  let helpP (p : sourcespan program) =
    match p with
    | Program (decls, e, tag) -> Program (List.map helpD decls, help e, tag)
  in
  helpP e
;;

(* Renames all variables appended with their unique tag so that no variable shadowing take place *)
let rec rename (e : tag program) : tag program =
  let rec help (env : (string * string) list) (e : tag expr) =
    match e with
    | EId (x, tag) -> EId (find env x, tag)
    | EBool (x, tag) -> EBool (x, tag)
    | ELet ((x, e, b_tag) :: rest, body, tag) ->
        let new_binding = sprintf "%s#%d" x b_tag in
        let new_env = (x, new_binding) :: env in
        let new_expr = help env e in
        ELet ([(new_binding, new_expr, b_tag)], help new_env (ELet (rest, body, tag)), tag)
    | ELet ([], body, _) -> help env body
    | ENumber (n, tag) -> ENumber (n, tag)
    | EPrim1 (op, e, tag) -> EPrim1 (op, help env e, tag)
    | EPrim2 (op, e1, e2, tag) -> EPrim2 (op, help env e1, help env e2, tag)
    | EIf (cond, thn, els, tag) -> EIf (help env cond, help env thn, help env els, tag)
    | EApp (funname, args, tag) -> EApp (funname, List.map (help env) args, tag)
  in
  let helpD (env : (string * string) list) (d : tag decl) : tag decl =
    match d with
    | DFun (name, args, body, tag) ->
        let renamed_args, new_env =
          List.fold_right
            (fun (arg, tag) (args, env) ->
              let new_arg = sprintf "%s#%d" arg tag in
              ((new_arg, tag) :: args, (arg, new_arg) :: env) )
            args ([], [])
        in
        DFun (name, renamed_args, help new_env body, tag)
  in
  let helpP (env : (string * string) list) (p : tag program) =
    match p with
    | Program (decls, e, tag) -> Program (List.map (helpD env) decls, help env e, tag)
  in
  helpP [] e
;;

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program (decls, body, _) -> AProgram (List.map helpD decls, helpA body, ())
  and helpD (d : tag decl) : unit adecl =
    match d with
    | DFun (name, args, body, _) -> ADFun (name, List.map fst args, helpA body, ())
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
    | ELet ((bind, exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EApp (funname, args, _) ->
        let args_ans, args_ctx =
          List.fold_right
            (fun b acc ->
              match acc with
              | ans_li, ctx_li ->
                  let ans, ctx = helpI b in
                  (ans :: ans_li, ctx @ ctx_li) )
            args ([], [])
        in
        (CApp (funname, args_ans, ()), args_ctx)
    | _ ->
        let imm, setup = helpI e in
        (CImmExpr imm, setup)
  and helpI (e : tag expr) : unit immexpr * (string * unit cexpr) list =
    match e with
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (name, _) -> (ImmId (name, ()), [])
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
    | EApp (funname, args, tag) ->
        let tmp = sprintf "fun_%d" tag in
        let arg_imms, arg_setups =
          List.fold_right
            (fun b acc ->
              match acc with
              | ans_li, ctx_li ->
                  let ans, ctx = helpI b in
                  (ans :: ans_li, ctx @ ctx_li) )
            args ([], [])
        in
        (ImmId (tmp, ()), arg_setups @ [(tmp, CApp (funname, arg_imms, ()))])
    | ELet ([], body, _) -> helpI body
    | ELet ((bind, exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
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
    | EApp (funname, args, s) -> (
      try
        let source, arity = List.assoc funname functions in
        let arg_exn_list = List.concat_map (wf_E bindings functions) args in
        let arg_length = List.length args in
        if arity = arg_length then
          arg_exn_list
        else
          [Arity (arity, arg_length, s)] @ arg_exn_list
      with Not_found -> [UnboundFun (funname, s)] )
    | ELet (binds, body, _) ->
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
  and wf_D (functions : (string * (sourcespan * int)) list) (d : sourcespan decl) :
      exn list * (string * (sourcespan * int)) list =
    match d with
    | DFun (funname, args, body, source) -> (
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
          let old_source, arity = List.assoc funname functions in
          ( exn_list @ (DuplicateFun (funname, source, old_source) :: wf_E args functions body),
            functions )
        with Not_found ->
          let new_functions = (funname, (source, List.length args)) :: functions in
          (exn_list @ wf_E args new_functions body, new_functions) )
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

(* ASSUMES that the program has been alpha-renamed and all names are unique *)
let naive_stack_allocation (prog : tag aprogram) : tag aprogram * arg envt =
  let rec helpAExpr (si : int) (e : tag aexpr) : arg envt =
    match e with
    | ALet (str, cexp, aexp, _) ->
        let new_arg = [(str, RegOffset (~-8 * si, RBP))] in
        new_arg
        @ helpCExpr (si + 1) cexp
        @ helpAExpr (si + 1) aexp 
    | ACExpr cexp -> helpCExpr si cexp
  and helpCExpr (si : int) (e : tag cexpr) : arg envt =
    match e with
    | CIf (_, thn_aexp, els_aexp, _) -> helpAExpr si thn_aexp @ helpAExpr si els_aexp
    | _ -> []
  and helpD (d : tag adecl) : arg envt =
    match d with
    | ADFun (_, args, body, _) ->
        let arg_length = List.length args - 1 in
        let arg_locs = List.mapi (fun i arg -> (arg, RegOffset ((8 * i) + 16, RBP))) args in
        arg_locs @ helpAExpr 1 body
  in
  match prog with
  | AProgram (decls, body, _) ->
      let decl_env = List.concat (List.map helpD decls) in
      let body_env = helpAExpr 1 body in
      (prog, decl_env @ body_env)
;;

(* In Cobra, you had logic somewhere that tracked the stack index, starting at 1 and incrementing it
   within the bodies of let-bindings.  It built up an environment that mapped each let-bound name to
   a stack index, so that RegOffset(~-8 * stackIndex, RBP) stored the value of that let-binding.
   In this function, you should build up that environment, and return a pair of the already-ANF'ed
   program and that environment, which can then both be passed along to compile_prog to finish compilation.

   Since this environment-building step comes *after* renaming, you may rely on the invariant that every
   name in the program appears bound exactly once, and therefore those names can be used as keys in
   an environment without worry of shadowing errors.
*)

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
  @ [IMov(Reg RSP, Reg RBP); ISub(Reg RSP, Const(Int64.of_int (8 * rounded_stack_number)))]

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
        ITest (Reg RAX, HexConst bool_tag);
        IJz "err_if_not_bool";
        IMov (Reg R11, const_false);
        ICmp (Reg RAX, Reg R11);
        IJe else_label ]
      @ compile_aexpr thn env num_args is_tail
      @ [IJmp done_label; ILabel else_label]
      @ compile_aexpr els env num_args is_tail
      @ [ILabel done_label]
  | CApp (funname, args, _) ->
      let args =
        if List.length args mod 2 = 1 then
          args @ [ImmNum (0L, 0)]
        else
          args
      in
      let push_args_instr =
        List.concat
          (List.rev
             (List.mapi (fun i imm -> [IMov (Reg R10, compile_imm imm env); IPush (Reg R10)]) args) )
      in
      let pop_args_instr = List.map (fun imm -> IPop (Reg R10)) args in
      let push_swap_instr =
        List.concat
          (List.rev
             (List.mapi (fun i arg -> [IMov (Reg RAX, compile_imm arg env); IPush (Reg RAX)]) args) )
      in
      let pop_swap_instr =
        List.concat
          (List.mapi
             (fun i arg -> [IPop (Reg RAX); IMov (RegOffset (16 + (i * 8), RBP), Reg RAX)])
             args )
      in
      if true && is_tail && List.length args <= num_args then
        push_swap_instr @ pop_swap_instr @ [IJmp (funname ^ "_body")]
      else
        push_args_instr @ [ICall funname] @ pop_args_instr
  | CImmExpr e -> [IMov (Reg RAX, compile_imm e env)]
  | CPrim1 (Add1, arg, _) ->
      [ IMov (Reg RAX, compile_imm arg env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IAdd (Reg RAX, Const 2L);
        IJo "err_overflow" ]
  | CPrim1 (Sub1, arg, _) ->
      [ IMov (Reg RAX, compile_imm arg env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        ISub (Reg RAX, Const 2L);
        IJo "err_overflow" ]
  | CPrim1 (Print, arg, _) ->
      [IMov (Reg RAX, compile_imm arg env); IMov (Reg RDI, Reg RAX); ICall "print"]
  | CPrim1 (IsBool, arg, tag) ->
      let label_tag = sprintf "is_bool_false#%d" tag in
      [ IMov (Reg RAX, compile_imm arg env);
        ITest (Reg RAX, HexConst bool_tag);
        IMov (Reg RAX, const_false);
        IJz label_tag;
        IMov (Reg RAX, const_true);
        ILabel label_tag ]
  | CPrim1 (IsNum, arg, tag) ->
      let label_tag = sprintf "is_num_true#%d" tag in
      [ IMov (Reg RAX, compile_imm arg env);
        ITest (Reg RAX, HexConst bool_tag);
        IMov (Reg RAX, const_true);
        IJz label_tag;
        IMov (Reg RAX, const_false);
        ILabel label_tag ]
  | CPrim1 (Not, arg, _) ->
      [ IMov (Reg RAX, compile_imm arg env);
        ITest (Reg RAX, HexConst bool_tag);
        IJz "err_logic_not_bool";
        IMov (Reg R11, bool_mask);
        IXor (Reg RAX, Reg R11) ]
  | CPrim1 (PrintStack, arg, _) ->
      [ (* Not fully implemented yet *)
        IMov (Reg RAX, compile_imm arg env);
        IMov (Reg RDI, Reg RAX);
        IMov (Reg RSI, Const (Int64.of_int num_args));
        ICall "printstack" ]
  | CPrim2 (Plus, l, r, _) ->
      let r_arg = compile_imm r env in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IMov (Reg R11, r_arg);
        IAdd (Reg RAX, Reg R11);
        IJo "err_overflow" ]
  | CPrim2 (Minus, l, r, _) ->
      let r_arg = compile_imm r env in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IMov (Reg R11, r_arg);
        ISub (Reg RAX, Reg R11);
        IJo "err_overflow" ]
  | CPrim2 (Times, l, r, _) ->
      let r_arg = compile_imm r env in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_arith_not_num";
        IMov (Reg R11, r_arg);
        ISar (Reg RAX, Const 1L);
        IMul (Reg RAX, Reg R11);
        IJo "err_overflow" ]
  | CPrim2 (And, l, r, _) ->
      let r_arg = compile_imm r env in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJz "err_logic_not_bool";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJz "err_logic_not_bool";
        IMov (Reg R11, r_arg);
        IAnd (Reg RAX, Reg R11) ]
  | CPrim2 (Or, l, r, _) ->
      let r_arg = compile_imm r env in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJz "err_logic_not_bool";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJz "err_logic_not_bool";
        IMov (Reg R11, r_arg);
        IOr (Reg RAX, Reg R11) ]
  | CPrim2 (Greater, l, r, tag) ->
      let r_arg = compile_imm r env in
      let jump_label = sprintf "greater#%d" tag in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg R11, r_arg);
        ICmp (Reg RAX, Reg R11);
        IMov (Reg RAX, const_true);
        IJg jump_label;
        IMov (Reg RAX, const_false);
        ILabel jump_label ]
  | CPrim2 (GreaterEq, l, r, tag) ->
      let r_arg = compile_imm r env in
      let jump_label = sprintf "greater_eq#%d" tag in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg RAX, compile_imm l env);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg R11, r_arg);
        ICmp (Reg RAX, Reg R11);
        IMov (Reg RAX, const_true);
        IJge jump_label;
        IMov (Reg RAX, const_false);
        ILabel jump_label ]
  | CPrim2 (Less, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "less#%d" tag in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg RAX, l_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg R11, r_arg);
        ICmp (Reg RAX, Reg R11);
        IMov (Reg RAX, const_true);
        IJl jump_label;
        IMov (Reg RAX, const_false);
        ILabel jump_label ]
  | CPrim2 (LessEq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "less_eq#%d" tag in
      [ IMov (Reg RAX, r_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg RAX, l_arg);
        ITest (Reg RAX, HexConst bool_tag);
        IJnz "err_comp_not_num";
        IMov (Reg R11, r_arg);
        ICmp (Reg RAX, Reg R11);
        IMov (Reg RAX, const_true);
        IJle jump_label;
        IMov (Reg RAX, const_false);
        ILabel jump_label ]
  | CPrim2 (Eq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "eq#%d" tag in
      [ IMov (Reg RAX, l_arg);
        IMov (Reg R11, r_arg);
        ICmp (Reg RAX, Reg R11);
        IMov (Reg RAX, const_true);
        IJe jump_label;
        IMov (Reg RAX, const_false);
        ILabel jump_label ]

and compile_imm (e : tag immexpr) (env : arg envt) =
  match e with
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find env x
;;

let compile_decl (env : arg envt) (d : tag adecl) : instruction list =
  match d with
  | ADFun (funname, args, aexp, tag) ->
      let compiled_fun = compile_fun funname args env aexp in
      let postlude = [IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet] in
      compiled_fun @ compile_aexpr aexp env (List.length args) true @ postlude
;;

let compile_prog ((anfed : tag aprogram), (env : arg envt)) : string =
  let prelude =
    "section .text\n\
     extern error\n\
     extern print\n\
     extern printstack\n\
     extern set_stack_bottom\n\
     global our_code_starts_here"
  in
  let postlude = [IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet] in
  let error_functions =
    [ ILineComment "Start of error functions";
      ILabel "err_arith_not_num";
      IMov (Reg RDI, Const 2L);
      IMov (Reg RSI, Reg RAX);
      ICall "error";
      ILabel "err_comp_not_num";
      IMov (Reg RDI, Const 1L);
      IMov (Reg RSI, Reg RAX);
      ICall "error";
      ILabel "err_if_not_bool";
      IMov (Reg RDI, Const 4L);
      IMov (Reg RSI, Reg RAX);
      ICall "error";
      ILabel "err_logic_not_bool";
      IMov (Reg RDI, Const 3L);
      IMov (Reg RSI, Reg RAX);
      ICall "error";
      ILabel "err_overflow";
      IMov (Reg RDI, Const 5L);
      IMov (Reg RSI, Reg RAX);
      ICall "error" ]
  in
  match anfed with
  | AProgram (decls, body_aexp, _) ->
      let decls_instr = List.concat (List.map (compile_decl env) decls) in
      let body_instr = compile_aexpr body_aexp env 0 true in
      let rounded_stack_number =
        let num_vars = deepest_stack body_aexp env in
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
      let body_stack_setup =
        [ILabel "our_code_starts_here"; IPush (Reg RBP); IMov (Reg RBP, Reg RSP)]
        @ pushes 0
        @ [IMov (Reg RDI, Reg RBP); IMov (Reg RSI, Reg RAX); ICall "set_stack_bottom"]
      in
      let as_assembly_string =
        to_asm (decls_instr @ body_stack_setup @ body_instr @ postlude @ error_functions)
      in
      sprintf "%s%s\n" prelude as_assembly_string
;;

(* Feel free to add additional phases to your pipeline.
   The final pipeline phase needs to return a string,
   but everything else is up to you. *)
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_err_phase well_formed is_well_formed
  |> add_phase desugared short_circuit_desugar
  |> add_phase tagged tag |> add_phase renamed rename
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase locate_bindings naive_stack_allocation
  |> add_phase result compile_prog
;;
