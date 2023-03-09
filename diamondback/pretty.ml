open Exprs
open Printf
open Format
open Lexing

let rec intersperse (elts : 'a list) (sep : 'a) : 'a list =
  match elts with
  | [] -> []
  | [elt] -> [elt]
  | elt::rest -> elt::sep::(intersperse rest sep)

let string_of_op1 op =
  match op with
  | Add1 -> "add1"
  | Sub1 -> "sub1"
  | Print -> "print"
  | PrintStack -> "printStack"
  | Not -> "!"
  | IsNum -> "isnum"
  | IsBool -> "isbool"

let name_of_op1 op =
  match op with
  | Add1 -> "Add1"
  | Sub1 -> "Sub1"
  | Print -> "Print"
  | PrintStack -> "PrintStack"
  | Not -> "Not"
  | IsNum -> "IsNum"
  | IsBool -> "IsBool"

let string_of_op2 op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | And -> "&&"
  | Or -> "||"
  | Greater -> ">"
  | Less -> "<"
  | GreaterEq -> ">="
  | LessEq -> "<="
  | Eq -> "=="
let name_of_op2 op =
  match op with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | And -> "And"
  | Or -> "Or"
  | Greater -> "Greater"
  | Less -> "Less"
  | GreaterEq -> "GreaterEq"
  | LessEq -> "LessEq"
  | Eq -> "Eq"
               

let rec string_of_expr_with (print_a : 'a -> string) (e : 'a expr) : string =
  let string_of_expr = string_of_expr_with print_a in
  match e with
  | ENumber(n, a) -> (Int64.to_string n) ^ (print_a a)
  | EBool(b, a) -> (string_of_bool b) ^ (print_a a)
  | EId(x, a) -> x ^ (print_a a)
  | EPrim1(op, e, a) ->
     sprintf "%s(%s)%s" (string_of_op1 op) (string_of_expr e) (print_a a)
  | EPrim2(op, left, right, a) ->
     sprintf "(%s %s %s)%s" (string_of_expr left) (string_of_op2 op) (string_of_expr right) (print_a a)
  | ELet(binds, body, a) ->
     let binds_strs = List.map (fun (x, e, _) -> sprintf "%s = %s" x (string_of_expr e)) binds in
     let binds_str = List.fold_left (^) "" (intersperse binds_strs ", ") in
     sprintf "(let %s in %s)%s" binds_str (string_of_expr body) (print_a a)
  | EIf(cond, thn, els, a) ->
     sprintf "(if %s: %s else: %s)%s"
             (string_of_expr cond)
             (string_of_expr thn)
             (string_of_expr els)
             (print_a a)
  | EApp(funname, args, a) ->
     sprintf "(%s(%s))%s" funname (ExtString.String.join ", " (List.map string_of_expr args)) (print_a a)
let string_of_expr (e : 'a expr) : string =
  string_of_expr_with (fun _ -> "") e
             
let string_of_decl_with (print_a : 'a -> string) (d : 'a decl) : string =
  match d with
  | DFun(name, args, body, a) ->
     sprintf "(def %s(%s):\n  %s)%s"
       name
       (ExtString.String.join ", " (List.map (fun (arg, a) -> sprintf "%s%s" arg (print_a a)) args))
       (string_of_expr_with print_a body) (print_a a)
let string_of_decl (d : 'a decl) : string =
  string_of_decl_with (fun _ -> "") d

let string_of_program_with (print_a : 'a -> string) (p : 'a program) : string =
  match p with
  | Program(decls, body, a) ->
     (ExtString.String.join "\n" (List.map (string_of_decl_with print_a) decls)) ^ "\n" ^
       (string_of_expr_with print_a body) ^ "\n" ^ (print_a a)
let string_of_program (p : 'a program) : string =
  string_of_program_with (fun _ -> "") p


let string_of_position (p : position) : string =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;

let string_of_sourcespan ((pstart, pend) : sourcespan) : string =
  sprintf "%s, %d:%d-%d:%d" pstart.pos_fname pstart.pos_lnum (pstart.pos_cnum - pstart.pos_bol)
          pend.pos_lnum (pend.pos_cnum - pend.pos_bol)

let rec string_of_aexpr_with (print_a : 'a -> string) (e : 'a aexpr) : string =
  let string_of_aexpr = string_of_aexpr_with print_a in
  match e with
  | ALet(x, e, b, a) -> sprintf "(alet %s = %s in %s)%s" x (string_of_cexpr_with print_a e) (string_of_aexpr b) (print_a a)
  | ACExpr c -> string_of_cexpr_with print_a c
and string_of_cexpr_with (print_a : 'a -> string) (c : 'a cexpr) : string =
  let string_of_aexpr = string_of_aexpr_with print_a in
  let string_of_immexpr = string_of_immexpr_with print_a in
  match c with
  | CPrim1(op, e, a) ->
     sprintf "%s(%s)%s" (string_of_op1 op) (string_of_immexpr e) (print_a a)
  | CPrim2(op, left, right, a) ->
     sprintf "(%s %s %s)%s" (string_of_immexpr left) (string_of_op2 op) (string_of_immexpr right) (print_a a)
  | CIf(cond, thn, els, a) ->
     sprintf "(if %s: %s else: %s)%s"
             (string_of_immexpr cond)
             (string_of_aexpr thn)
             (string_of_aexpr els)
             (print_a a)
  | CApp(funname, args, a) ->
     sprintf "(%s(%s))%s" funname (ExtString.String.join ", " (List.map string_of_immexpr args)) (print_a a)
  | CImmExpr i -> string_of_immexpr i
and string_of_immexpr_with (print_a : 'a -> string) (i : 'a immexpr) : string =
  match i with
  | ImmNum(n, a) -> (Int64.to_string n) ^ (print_a a)
  | ImmBool(b, a) -> (string_of_bool b) ^ (print_a a)
  | ImmId(x, a) -> x ^ (print_a a)
and string_of_aprogram_with (print_a : 'a -> string) (p : 'a aprogram) : string =
  match p with
  | AProgram(decls, body, a) ->
     (ExtString.String.join "\n" (List.map (string_of_adecl_with print_a) decls)) ^ "\n"
     ^ (string_of_aexpr_with print_a body) ^ "\n" ^ (print_a a)
and string_of_adecl_with (print_a : 'a -> string) (d : 'a adecl) : string =
  match d with
  | ADFun(name, args, body, a) ->
     sprintf "(fun %s(%s): %s)%s" name (ExtString.String.join ", " args)
       (string_of_aexpr_with print_a body) (print_a a)

let string_of_aexpr (e : 'a aexpr) : string = string_of_aexpr_with (fun _ -> "") e
let string_of_cexpr (c : 'a cexpr) : string = string_of_cexpr_with (fun _ -> "") c
let string_of_immexpr (i : 'a immexpr) : string = string_of_immexpr_with (fun _ -> "") i
let string_of_adecl (d : 'a adecl) : string = string_of_adecl_with (fun _ -> "") d
let string_of_aprogram (p : 'a aprogram) : string = string_of_aprogram_with (fun _ -> "") p

             
let format_expr (e : 'a expr) (print_a : 'a -> string) : string =
  let maybe_a a =
    let astr = print_a a in
    if astr = "" then "" else "<" ^ astr ^ ">" in
  let indent = 2 in
  let print_list fmt p_item items p_sep =
    match items with
    | [] -> ();
    | [item] -> p_item fmt item
    | first::rest ->
       p_item fmt first;
       List.iter (fun item -> p_sep fmt; p_item fmt item) rest in
  let print_comma_sep fmt =
    pp_print_string fmt ","; pp_print_space fmt () in
  let open_label fmt label a =
    pp_open_hvbox fmt indent; pp_print_string fmt label; pp_print_string fmt (maybe_a a);
    pp_print_string fmt "("; pp_print_cut fmt () in
  let open_paren fmt =
    pp_open_box fmt 2; pp_print_string fmt "("; pp_print_cut fmt () in
  let close_paren fmt =
    pp_print_break fmt 0 (~-indent); pp_close_box fmt (); pp_print_string fmt ")" in
  let quote x = "\"" ^ x ^ "\"" in
  let rec help e fmt =
    match e with
    | ENumber(n, a) ->
       open_label fmt "ENumber" a;
       pp_print_string fmt (Int64.to_string n);
       close_paren fmt
    | EBool(b, a) ->
       open_label fmt "EBool" a;
       pp_print_bool fmt b;
       close_paren fmt
    | EId(x, a) ->
       open_label fmt "EId" a;
       pp_print_string fmt (quote x);
       close_paren fmt
    | EPrim1(op, e, a) ->
       open_label fmt "EPrim1" a;
       pp_print_string fmt (name_of_op1 op);
       print_comma_sep fmt; help e fmt; 
       close_paren fmt
    | EPrim2(op, e1, e2, a) ->
       open_label fmt "EPrim2" a;
       pp_print_string fmt (name_of_op2 op);
       print_comma_sep fmt; help e1 fmt; print_comma_sep fmt; help e2 fmt;
       close_paren fmt
    | EIf(cond, thn, els, a) ->
       open_label fmt "EIf" a;
       help cond fmt; print_comma_sep fmt; help thn fmt; print_comma_sep fmt; help els fmt;
       close_paren fmt
    | EApp(funname, args, a) ->
       open_label fmt "EApp" a;
       pp_print_string fmt (quote funname);
       print_comma_sep fmt;
       (match args with
        | [] -> ()
        | [e] -> help e fmt
        | e1::rest -> help e1 fmt; List.iter (fun e -> print_comma_sep fmt; help e fmt) rest);
       close_paren fmt
    | ELet(binds, body, a) ->
       let print_item fmt (x, b, a) =
         open_paren fmt;
         pp_print_string fmt (" " ^ (quote x)); pp_print_string fmt (maybe_a a); print_comma_sep fmt; help b fmt;
         close_paren fmt in
       open_label fmt "ELet" a;
       open_paren fmt; print_list fmt print_item binds print_comma_sep; close_paren fmt;
       print_comma_sep fmt;
       help body fmt;
       close_paren fmt
  in
  help e str_formatter;
  flush_str_formatter ()
;;
     
    
let rec ast_of_pos_expr (e : (Lexing.position * Lexing.position) expr) : string =
  format_expr e string_of_sourcespan
let rec ast_of_expr (e : 'a expr) : string =
  format_expr e (fun _ -> "")

