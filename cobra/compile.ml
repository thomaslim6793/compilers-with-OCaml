open Printf
open Errors
open Exprs
open Pretty
open Phases

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;


let const_true    = HexConst(0xFFFFFFFFFFFFFFFFL)
let const_false   = HexConst(0x7FFFFFFFFFFFFFFFL)
let bool_mask     = HexConst(0x8000000000000000L)
let bool_tag      = 0x0000000000000001L
let bool_tag_mask = 0x0000000000000007L
let num_tag       = 0x0000000000000000L
let num_tag_mask  = 0x0000000000000001L

let err_COMP_NOT_NUM   = 1L
let err_ARITH_NOT_NUM  = 2L
let err_LOGIC_NOT_BOOL = 3L
let err_IF_NOT_BOOL    = 4L
let err_OVERFLOW       = 5L

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]


let check_scope (e : sourcespan expr) : sourcespan expr =
  let rec help e env =
    match e with
    | EBool _ -> ()
    | ENumber _ -> ()
    | EId (x, loc) ->
       (try ignore (List.assoc x env)
        with Not_found ->
             raise (BindingError(sprintf "The identifier %s, used at <%s>, is not in scope" x (string_of_sourcespan loc))))
    | EPrim1(_, e, _) -> help e env
    | EPrim2(_, l, r, _) -> help l env; help r env
    | EIf(c, t, f, _) -> help c env; help t env; help f env
    | ELet(binds, body, _) ->
       let (env2, _) =
         (List.fold_left
           (fun (scope_env, shadow_env) (x, e, loc) ->
             try
               let existing = List.assoc x shadow_env in
               raise (BindingError(sprintf "The identifier %s, defined at <%s>, shadows one defined at <%s>"
                                           x (string_of_sourcespan loc) (string_of_sourcespan existing)))
             with Not_found ->
               help e scope_env;
               ((x, loc)::scope_env, (x, loc)::shadow_env))
           (env, []) binds) in
       help body env2
  in help e []; e

let rec find (ls : (string * 'a) list) (x : string) : 'a =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y,v)::rest ->
     if y = x then v else find rest x

(* Renames all variables appended with their unique tag so that no variable shadowing take place *)
let rec rename (e : tag expr) : tag expr =
  let rec help (env : (string * string) list) (e : tag expr) =
    match e with
    | EId (x, tag) -> EId (find env x, tag)
    | EBool (x, tag) -> EBool(x, tag)
    | ELet ((x, e, b_tag) :: rest, body, tag) ->
        let new_binding = sprintf "%s#%d" x b_tag in
        let new_env = (x, new_binding) :: env in
        let new_expr = help new_env e in
        ELet ([(new_binding, new_expr, b_tag)], help new_env (ELet (rest, body, tag)), tag)
    | ELet ([], body, _) -> help env body
    | ENumber (n, tag) -> ENumber (n, tag)
    | EPrim1 (op, e, tag) -> EPrim1 (op, help env e, tag)
    | EPrim2 (op, e1, e2, tag) -> EPrim2 (op, help env e1, help env e2, tag)
    | EIf (cond, thn, els, tag) -> EIf (help env cond, help env thn, help env els, tag)
  in
  help [] e
;;

let tag (e : 'a expr) : tag expr =
  let rec help (e : 'a expr) (num : int) : (tag expr * tag) =
    match e with
    | EId(x, _) -> (EId(x, num), num + 1)
    | ENumber(n, _) -> (ENumber(n, num), num + 1)
    | EBool(b, _) -> (EBool(b, num), num + 1)
    | EPrim1(op, e, _) ->
       let (tag_e, new_n) = help e (num + 1) in
       (EPrim1(op, tag_e, num), new_n)
    | EPrim2(op, e1, e2, _) ->
       let (tag_e1, num_e1) = help e1 (num + 1) in
       let (tag_e2, num_e2) = help e2 (num_e1) in
       (EPrim2(op, tag_e1, tag_e2, num), num_e2)
    | ELet(binds, body, _) ->
       let (new_binds, num_binds) =
         List.fold_left
           (fun (rev_binds, next_num) (x, b, _) ->
             let (tag_b, num_b) = help b (next_num + 1) in
             ((x, tag_b, next_num)::rev_binds, num_b))
           ([], num + 1) binds in
       let (tag_body, num_body) = help body num_binds in
       (ELet(List.rev new_binds, tag_body, num), num_body)
    | EIf(cond, thn, els, _) ->
       let (tag_cond, num_cond) = help cond (num + 1) in
       let (tag_thn, num_thn) = help thn (num_cond) in
       let (tag_els, num_els) = help els (num_thn) in
       (EIf(tag_cond, tag_thn, tag_els, num), num_els)
  in let (ans, _) = help e 1
     in ans

let rec untag (e : 'a expr) : unit expr =
  match e with
  | EId(x, _) -> EId(x, ())
  | ENumber(n, _) -> ENumber(n, ())
  | EBool(b, _) -> EBool(b, ())
  | EPrim1(op, e, _) ->
     EPrim1(op, untag e, ())
  | EPrim2(op, e1, e2, _) ->
     EPrim2(op, untag e1, untag e2, ())
  | ELet(binds, body, _) ->
     ELet(List.map(fun (x, b, _) -> (x, untag b, ())) binds, untag body, ())
  | EIf(cond, thn, els, _) ->
     EIf(untag cond, untag thn, untag els, ())

let anf (e : tag expr) : unit expr =
  let rec helpC (e : tag expr) : (unit expr * (string * unit expr) list) = 
    match e with
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (EPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (match op with
       | And 
       | Or -> (EPrim2(op, left_imm, anf right, ()), left_setup)
       | _ -> (EPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup))
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (EIf(cond_imm, anf _then, anf _else, ()), cond_setup)
    | ENumber(n, _) -> (ENumber(n, ()), [])
    | EBool(b, _) -> (EBool(b, ()), [])
    | ELet([], body, _) -> helpC body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EId(name, _) -> (EId(name, ()), [])
  and helpI (e : tag expr) : (unit expr * (string * unit expr) list) =
    match e with
    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (EId(tmp, ()), arg_setup @ [(tmp, EPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (match op with
       | And 
       | Or -> (EId(tmp, ()), left_setup @ [(tmp, EPrim2(op, left_imm, anf right, ()))])
       | _ -> (EId(tmp, ()), left_setup @ right_setup @ [(tmp, EPrim2(op, left_imm, right_imm, ()))]))
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (EId(tmp, ()), cond_setup @ [(tmp, EIf(cond_imm, anf _then, anf _else, ()))])
    | ENumber(n, _) -> (ENumber(n, ()), [])
    | EBool(b, _) -> (EBool(b, ()), [])
    | ELet([], body, _) -> helpI body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EId(name, _) -> (EId(name, ()), [])
  and anf e = 
    let (ans, ans_setup) = helpI e in
    List.fold_right (fun (bind, exp) body -> ELet([bind, exp, ()], body, ())) ans_setup ans
  in
  anf e
;;

  
let r_to_asm (r : reg) : string =
  match r with
  | RAX -> "RAX"
  | RSI -> "RSI"
  | RDI -> "RDI"
  | RCX -> "RCX"
  | RDX -> "RDX"
  | RSP -> "RSP"
  | RBP -> "RBP"
  | R8  -> "R8"
  | R9  -> "R9"
  | R11 -> "R11"

let rec arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%Ld" n
  | HexConst(n) -> sprintf "QWORD 0x%Lx" n
  | Reg(r) -> r_to_asm r
  | RegOffset(n, r) ->
     if n >= 0 then
       sprintf "[%s+%d]" (r_to_asm r) (word_size * n)
     else
       sprintf "[%s-%d]" (r_to_asm r) (-1 * word_size * n)
  | Sized(size, a) ->
     sprintf "%s %s"
             (match size with | QWORD_PTR -> "QWORD" | DWORD_PTR -> "DWORD" | WORD_PTR -> "WORD" | BYTE_PTR -> "BYTE")
             (arg_to_asm a)

let rec i_to_asm (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
     sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd(dest, to_add) ->
     sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | ISub(dest, to_sub) ->
     sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
  | IMul(dest, to_mul) ->
     sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
  | ICmp(left, right) ->
     sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
  | ILabel(name) ->
     name ^ ":"
  | IJo(label) ->
     sprintf "  jo %s" label
  | IJe(label) ->
     sprintf "  je %s" label
  | IJne(label) ->
     sprintf "  jne %s" label
  | IJl(label) ->
     sprintf "  jl %s" label
  | IJle(label) ->
     sprintf "  jle %s" label
  | IJg(label) ->
     sprintf "  jg %s" label
  | IJge(label) ->
     sprintf "  jge %s" label
  | IJmp(label) ->
     sprintf "  jmp %s" label
  | IJz(label) ->
     sprintf "  jz %s" label
  | IJnz(label) ->
     sprintf "  jnz %s" label
  | IAnd(dest, value) ->
     sprintf "  and %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IOr(dest, value) ->
     sprintf "  or %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IXor(dest, value) ->
     sprintf "  xor %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IShl(dest, value) ->
     sprintf "  shl %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IShr(dest, value) ->
     sprintf "  shr %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | ISar(dest, value) ->
     sprintf "  sar %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IPush(value) ->
     sprintf "  push %s" (arg_to_asm value)
  | IPop(dest) ->
     sprintf "  pop %s" (arg_to_asm dest)
  | ICall(label) ->
     sprintf "  call %s" label
  | IRet ->
     "  ret"
  | ITest(arg, comp) ->
     sprintf "  test %s, %s" (arg_to_asm arg) (arg_to_asm comp)
  | ILineComment(str) ->
     sprintf "  ;; %s" str
  | IInstrComment(instr, str) ->
     sprintf "%s ; %s" (i_to_asm instr) str

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is


(* NOTE: Assumes that e is in ANF *)
let rec count_vars (e : 'a expr) =
  match e with
  | EIf(_, t, f, _) -> max (count_vars t) (count_vars f)
  | ELet([_, b, _], body, _) ->
     1 + (max (count_vars b) (count_vars body))
  | _ -> 0

let rec replicate (x : 'a) (i : int) : 'a list =
  if i = 0 then []
  else x :: (replicate x (i - 1))


let rec compile_expr (e : tag expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | ELet([id, e, _], body, _) ->
     let prelude = compile_expr e (si + 1) env in
     let body = compile_expr body (si + 1) ((id, si)::env) in
     prelude
     @ [ IMov(RegOffset(~-si, RBP), Reg(RAX)) ]
     @ body
  | EPrim1 _ -> compile_prim1 e si env
  | EPrim2 _ -> compile_prim2 e si env
  | EIf (cond, thn, els, tag) ->
     let else_label = sprintf "if_false_%d" tag in
     let done_label = sprintf "done_%d" tag in
      [
         IMov(Reg RAX, compile_imm cond env);
         ITest(Reg RAX, HexConst(bool_tag));
         IJz("err_if_not_bool");
         IMov(Reg R11, const_false);
         ICmp (Reg RAX, Reg R11); 
         IJe else_label
      ]
      @ compile_expr thn si env
      @ [IJmp done_label; ILabel else_label]
      @ compile_expr els si env @ [ILabel done_label]

  | ENumber(n, _) -> [ IMov(Reg(RAX), compile_imm e env) ]
  | EBool(n, _) -> [ IMov(Reg(RAX), compile_imm e env) ]
  | EId(x, _) -> [ IMov(Reg(RAX), compile_imm e env) ]
  | _ -> raise (InternalCompilerError "Impossible: Not in ANF")
and compile_imm (e : tag expr) (env : (string * int) list) : arg =
  match e with
  | ENumber(n, _) ->
     if n > (Int64.div Int64.max_int 2L) || n < (Int64.div Int64.min_int 2L) then
       (* TODO: raise a better error of your choosing here *)
       failwith ("Integer overflow: " ^ (Int64.to_string n))
     else
      Const(Int64.shift_left n 1) 
  | EBool(true, _) -> const_true
  | EBool(false, _) -> const_false
  | EId(x, _) -> RegOffset(~-(find env x), RBP)
  | _ -> raise (InternalCompilerError "Impossible: not an immediate")
and compile_prim1 (e : tag expr) (si : int) (env : (string * int) list) : instruction list = 
  match e with
  | EPrim1(Add1, arg, tag) ->
    let arg_expr = compile_expr arg si env in
    arg_expr
    @ [
      ITest(Reg RAX, HexConst(bool_tag));
      IJnz("err_arith_not_num");
      IAdd (Reg RAX, Const 2L);
      IJo("err_overflow")
      ]
  | EPrim1(Sub1, arg, tag) ->
    let arg_expr = compile_expr arg si env in
    arg_expr
    @ [
      ITest(Reg RAX, HexConst(bool_tag));
      IJnz("err_arith_not_num");
      ISub (Reg RAX, Const 2L);
      IJo("err_overflow")
      ]
  | EPrim1(Print, arg, tag) ->
   let arg_instr = compile_expr arg si env in
   arg_instr
   @ 
   [
      IMov(Reg RDI, Reg RAX);
      ICall("print")
   ]
  | EPrim1(IsBool, arg, tag) ->
   let arg_expr = compile_expr arg si env in
   let label_tag = sprintf "is_bool_false#%d" tag in
   arg_expr
   @ 
   [
      ITest(Reg RAX, HexConst(bool_tag));
      IMov(Reg RAX, const_false);
      IJz(label_tag);
      IMov(Reg RAX, const_true);
      ILabel(label_tag)
   ]
  | EPrim1(IsNum, arg, tag) ->
   let arg_expr = compile_expr arg si env in
   let label_tag = sprintf "is_num_true#%d" tag in
   arg_expr
   @ [ITest(Reg RAX, HexConst(bool_tag));
   IMov(Reg RAX, const_true);
   IJz(label_tag);
   IMov(Reg RAX, const_false);
   ILabel(label_tag)
   ]
  | EPrim1(Not, arg, tag) ->
   let arg_instr = compile_expr arg si env in
   arg_instr
   @ [
      ITest(Reg RAX, HexConst(bool_tag)); 
      IJz("err_logic_not_bool");
      IMov(Reg R11, bool_mask);
      IXor (Reg RAX, Reg R11)
      ]
  | EPrim1(PrintStack, arg, tag) ->
   let arg_instr = compile_expr arg si env in
   arg_instr
   @ [
      IMov(Reg RDI, Reg RSP);
      IMov(Reg RSI, Reg RAX);
      ICall("printstack")
   ] 
  | _ -> raise (InternalCompilerError("Expecting a Prim1 in compile_prim1"))
and compile_prim2 (e : tag expr) (si : int) (env : (string * int) list) : instruction list = 
   match e with
   | EPrim2 (Plus, arg1, arg2, tag) -> 
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_arith_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_arith_not_num");
        IMov(Reg R11, r_arg);
        IAdd(Reg RAX, Reg R11);
        IJo("err_overflow")
      ]
   | EPrim2 (Minus, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_arith_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_arith_not_num");
        IMov(Reg R11, r_arg);
        ISub(Reg RAX, Reg R11);
        IJo("err_overflow")
      ]
   | EPrim2 (Times, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_arith_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_arith_not_num");
        ISar(Reg RAX, Const 1L);
        IMov(Reg R11, r_arg);
        IMul(Reg RAX, Reg R11);
        IJo("err_overflow")
      ]
   | EPrim2 (And, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let if_instr = compile_expr (EIf(arg1, arg2, EBool(false, tag), tag)) si env in
      [
         IMov (Reg R11, l_arg);
         ITest (Reg R11, HexConst bool_tag);
         IJz ("err_logic_not_bool");
      ] @
      if_instr @
      [
         ITest (Reg RAX, HexConst bool_tag);
         IJz ("err_logic_not_bool");
      ]
   | EPrim2 (Or, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let if_instr = compile_expr (EIf(arg1, EBool(true, tag), arg2, tag)) si env in
      [
         IMov (Reg R11, l_arg);
         ITest (Reg R11, HexConst bool_tag);
         IJz ("err_logic_not_bool");
      ] @
      if_instr @
      [
         ITest (Reg RAX, HexConst bool_tag);
         IJz ("err_logic_not_bool");
      ]
   | EPrim2 (Greater, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "greater#%d" tag in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX,  HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg R11, r_arg);
        ICmp(Reg RAX, Reg R11);
        IMov(Reg RAX, const_true);
        IJg(jump_label);
        IMov(Reg RAX, const_false);
        ILabel(jump_label)
      ]
   | EPrim2 (GreaterEq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "greater_eq#%d" tag in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg R11, r_arg);
        ICmp(Reg RAX, Reg R11); 
        IMov(Reg RAX, const_true);
        IJge(jump_label);
        IMov(Reg RAX, const_false);
        ILabel(jump_label)
      ]
   | EPrim2 (Less, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "less#%d" tag in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX,HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg R11, r_arg);
        ICmp(Reg RAX, Reg R11);
        IMov(Reg RAX, const_true);
        IJl(jump_label);
        IMov(Reg RAX, const_false);
        ILabel(jump_label)
      ]
   | EPrim2 (LessEq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "less_eq#%d" tag in
      [
        IMov(Reg RAX, r_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg RAX, l_arg); 
        ITest(Reg RAX, HexConst(bool_tag));
        IJnz("err_comp_not_num");
        IMov(Reg R11, r_arg);
        ICmp(Reg RAX, Reg R11);
        IMov(Reg RAX, const_true);
        IJle(jump_label);
        IMov(Reg RAX, const_false);
        ILabel(jump_label)
      ]
   | EPrim2 (Eq, arg1, arg2, tag) ->
      let l_arg = compile_imm arg1 env in
      let r_arg = compile_imm arg2 env in
      let jump_label = sprintf "eq#%d" tag in
      [
        IMov(Reg RAX, l_arg);
        IMov(Reg R11, r_arg); (* Why does this work over using stack index cmp ?*)
        ICmp(Reg RAX, Reg R11);
        IMov(Reg RAX, const_true);
        IJe(jump_label);
        IMov(Reg RAX, const_false);
        ILabel(jump_label)
      ]
   | _ -> raise (InternalCompilerError("Expecting a Prim2 in compile_prim2"))

;;

let compile_prog (anfed : tag expr) : string =
  let prelude =
    "section .text\nextern error\nextern print\nextern printstack\nglobal our_code_starts_here\nour_code_starts_here:" in
  let rounded_stack_number = 
   let num_vars = count_vars anfed in
   if num_vars mod 2 = 0 then num_vars
   else num_vars + 1
in
  let stack_setup = [
      (* FILL: insert instructions for setting up stack here *)
      IPush(Reg(RBP)); 
      IMov(Reg(RBP), Reg(RSP));
      ISub(Reg(RSP), Const((Int64.of_int (rounded_stack_number * 8)))) 
    ] in
  let postlude = [
      IMov(Reg(RSP), Reg(RBP));
      IPop(Reg(RBP));
      IRet
     ] in
  let error_functions = [
   ILineComment("Start of error functions");
   ILabel("err_arith_not_num");
   IMov(Reg RDI, Const 2L);
   IMov(Reg RSI, Reg RAX);
   ICall("error");
   ILabel("err_comp_not_num");
   IMov(Reg RDI, Const 1L);
   IMov(Reg RSI, Reg RAX);
   ICall("error");
   ILabel("err_if_not_bool");
   IMov(Reg RDI, Const 4L);
   IMov(Reg RSI, Reg RAX);
   ICall("error");
   ILabel("err_logic_not_bool");
   IMov(Reg RDI, Const 3L);
   IMov(Reg RSI, Reg RAX);
   ICall("error");
   ILabel("err_overflow");
   IMov(Reg RDI, Const 5L);
   IMov(Reg RSI, Reg RAX);
   ICall("error");
  ] in
  let body = (compile_expr anfed 1 []) in
  let as_assembly_string = (to_asm (stack_setup @ body @ postlude @ error_functions)) in
  sprintf "%s%s\n" prelude as_assembly_string 

let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> (add_phase well_formed check_scope)
  |> (add_phase tagged tag)
  |> (add_phase renamed rename)
  |> (add_phase anfed (fun p -> tag (anf p)))
  |> (add_phase result compile_prog)
;;
