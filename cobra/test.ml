open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs


(* Runs a program, given as a source string, and compares its output to expected *)
let t (name : string) (program : string) (expected : string) = name>::test_run program name expected;;

(* Runs a program, given as an ANFed expr, and compares its output to expected *)
let ta (name : string) (program : tag expr) (expected : string) = name>::test_run_anf program name expected;;

(* Runs a program, given as a source string, and compares its error to expected *)
let te (name : string) (program : string) (expected_err : string) = name>::test_err program name expected_err;;

(* Transforms a program into ANF, and compares the output to expected *)
let tanf (name : string) (program : 'a expr) (expected : unit expr) = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_expr;;

(* Renames all the values in a program to be unique *)
let trename (name : string) (program : 'a expr) (expected : tag expr) =
  name >:: fun _ -> assert_equal expected (rename (tag program)) ~printer:string_of_expr
;;

(* Tags a program such that all of the exprs are uniquely tagged  *)
let ttag (name : string) (program : 'a expr) (expected : tag expr) =
  name >:: fun _ -> assert_equal expected (tag program) ~printer:string_of_expr
;;

(* Checks if two strings are equal *)
let teq (name : string) (actual : string) (expected : string) = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

(* Runs a program, given as the name of a file in the input/ directory, and compares its output to expected *)
let tprog (filename : string) (expected : string) = filename>::test_run_input filename expected;;

(* Runs a program, given as the name of a file in the input/ directory, and compares its error to expected *)
let teprog (filename : string) (expected : string) = filename>::test_err_input filename expected;;

let forty = "let x = 40 in x"
let fals = "let x = false in x"
let tru = "let x = true in x"

(*
Testing notes for Cobra:
- Int overflow
- Negative/Positive high number that would otherwise LSL sign bit and change parity (line ~320)
- Test for prim1/prim2 overflow

*)

let signed_int63_max = 4611686018427387903
let signed_int63_min = -4611686018427387904

let forty_one = "41"

let forty_one_a = ENumber (41L, ())

(* Tests by Thomas *)
let thomas_tests =
  [ (* check_scope tests *)
    te "scope_test1" "let x = 1, x = 2 in x" "bound twice";
    te "scope_test2" "let x = 1, x = 2, x = 3 in x" "bound twice";
    te "scope_test3" "let x = 1, x = 2, y = 3 in x" "bound twice";
    te "scope_test4" "let x = 1 in x + (let y = 1, y = 2 in y)" "bound twice";
    te "scope_test5" "let x = let y = 1, y = 2 in y in x" "bound twice";
    te "scope_test6" "let x = 1, y = 2, x = 3 in x + y" "bound twice";
    te "scope_test7" "let x = 1 in y" "not bound";
    te "scope_test8" "let x = 1, y = 2 in x + y + z" "not bound";
    te "scope_test9" "let x = let x = 1 in y in x" "not bound";
    te "scope_test10" "if (let x = 1 in y) : 2 else: 3" "not bound";
    te "scope_test11" "if (let x = 1 in x) : (let y = x in y) else: 3" "not bound";
    te "scope_test12" "if (let x = 1 in x) : 2 else: (let y = x in y)" "not bound";
    te "scope_test13" "if (let x = 1 in x) : (let x = 2, y = 3 in x + y) else: (let z = 4 in w)"
      "not bound";
    te "scope_test14" "let y = (if (let x = 1 in y) : 2 else: 3) in y" "not bound";
    te "scope_test15" "let x = (if (let y = 1 in y) : 2 else: 3) in y" "not bound";
    (* the following should pass, specifically not have free variable issues *)
    t "scope_test16" "let x = 1 in (let y = 2 in x + y)" "3";
    t "scope_test17" "let x = (let x = 1 in x) in x" "1";
    t "scope_test18" "let x = 1 in (let y = 1 in y)" "1";
    t "scope_test19" "let x = 1 in x" "1" ]
;;

let rename_test_list =
  [ trename "rename_test1" (EPrim1 (Sub1, ENumber (55L, 2), 1)) (EPrim1 (Sub1, ENumber (55L, 2), 1));
    trename "rename_test2"
      (ELet
         ( [("x", EPrim2 (Times, ENumber (55L, 4), ENumber (55L, 5), 3), 2)],
           EPrim1 (Add1, EId ("x", 7), 6),
           1 ) )
      (ELet
         ( [("x#2", EPrim2 (Times, ENumber (55L, 4), ENumber (55L, 5), 3), 2)],
           EPrim1 (Add1, EId ("x#2", 7), 6),
           1 ) );
    trename "rename_test3"
      (ELet
         ( [("x", ENumber (55L, 3), 2)],
           ELet
             ( [("x", ENumber (55L, 6), 5)],
               ELet
                 ( [("x", ENumber (55L, 9), 8)],
                   ELet ([("x", ENumber (55L, 12), 11)], EPrim1 (Add1, EId ("x", 14), 13), 10),
                   7 ),
               4 ),
           1 ) )
      (ELet
         ( [("x#2", ENumber (55L, 3), 2)],
           ELet
             ( [("x#5", ENumber (55L, 6), 5)],
               ELet
                 ( [("x#8", ENumber (55L, 9), 8)],
                   ELet ([("x#11", ENumber (55L, 12), 11)], EPrim1 (Add1, EId ("x#11", 14), 13), 10),
                   7 ),
               4 ),
           1 ) ) ]
;;

let tag_test_list =
  [ ttag "tag_test1" (EPrim1 (Sub1, ENumber (55L, ()), ())) (EPrim1 (Sub1, ENumber (55L, 2), 1));
    ttag "tag_test2" (EId ("x", ())) (EId ("x", 1));
    ttag "tag_test3" (ENumber (55L, ())) (ENumber (55L, 1));
    ttag "tag_test4" (EPrim1 (Sub1, ENumber (55L, ()), ())) (EPrim1 (Sub1, ENumber (55L, 2), 1));
    ttag "tag_test5"
      (EPrim2 (Plus, ENumber (55L, ()), ENumber (55L, ()), ()))
      (EPrim2 (Plus, ENumber (55L, 2), ENumber (55L, 3), 1));
    ttag "tag_test6"
      (EPrim2 (Minus, ENumber (55L, ()), ENumber (55L, ()), ()))
      (EPrim2 (Minus, ENumber (55L, 2), ENumber (55L, 3), 1));
    ttag "tag_test7"
      (EPrim2 (Times, ENumber (55L, ()), ENumber (55L, ()), ()))
      (EPrim2 (Times, ENumber (55L, 2), ENumber (55L, 3), 1));
    ttag "tag_test8"
      (ELet ([("x", ENumber (55L, ()), ())], EPrim1 (Add1, EId ("x", ()), ()), ()))
      (ELet ([("x", ENumber (55L, 3), 2)], EPrim1 (Add1, EId ("x", 5), 4), 1));
    ttag "tag_test9"
      (ELet
         ( [("x", EPrim2 (Times, ENumber (55L, ()), ENumber (55L, ()), ()), ())],
           EPrim1 (Add1, EId ("x", ()), ()),
           () ) )
      (ELet
         ( [("x", EPrim2 (Times, ENumber (55L, 4), ENumber (55L, 5), 3), 2)],
           EPrim1 (Add1, EId ("x", 7), 6),
           1 ) );
    ttag "tag_test10"
      (EIf (ENumber (1L, ()), ENumber (2L, ()), ENumber (3L, ()), ()))
      (EIf (ENumber (1L, 2), ENumber (2L, 3), ENumber (3L, 4), 1));
    ttag "tag_test11"
      (EIf
         ( EIf (ENumber (1L, ()), ENumber (2L, ()), ENumber (3L, ()), ()),
           ELet ([("x", ENumber (55L, ()), ())], EPrim1 (Add1, EId ("x", ()), ()), ()),
           EPrim2 (Plus, ENumber (55L, ()), ENumber (55L, ()), ()),
           () ) )
      (EIf
         ( EIf (ENumber (1L, 3), ENumber (2L, 4), ENumber (3L, 5), 2),
           ELet ([("x", ENumber (55L, 8), 7)], EPrim1 (Add1, EId ("x", 10), 9), 6),
           EPrim2 (Plus, ENumber (55L, 12), ENumber (55L, 13), 11),
           1 ) );
    ttag "tag_test12"
      (EPrim2
         ( Times,
           EPrim2 (Times, ENumber (55L, ()), ENumber (55L, ()), ()),
           EPrim2 (Times, ENumber (55L, ()), ENumber (55L, ()), ()),
           () ) )
      (EPrim2
         ( Times,
           EPrim2 (Times, ENumber (55L, 3), ENumber (55L, 4), 2),
           EPrim2 (Times, ENumber (55L, 6), ENumber (55L, 7), 5),
           1 ) );
    ttag "tag_test13"
      (ELet
         ( [("x", ENumber (55L, ()), ())],
           ELet
             ( [("x", ENumber (55L, ()), ())],
               ELet
                 ( [("x", ENumber (55L, ()), ())],
                   ELet ([("x", ENumber (55L, ()), ())], EPrim1 (Add1, EId ("x", ()), ()), ()),
                   () ),
               () ),
           () ) )
      (ELet
         ( [("x", ENumber (55L, 3), 2)],
           ELet
             ( [("x", ENumber (55L, 6), 5)],
               ELet
                 ( [("x", ENumber (55L, 9), 8)],
                   ELet ([("x", ENumber (55L, 12), 11)], EPrim1 (Add1, EId ("x", 14), 13), 10),
                   7 ),
               4 ),
           1 ) ) ]
;;

let prim1_tests = 
  [
      t "add1_test1" "add1(1)" "2";
      t "add1_test2" "add1(add1(1))" "3";
      t "add1_test3" "add1(add1(-1))" "1";
      t "add1_test4" "add1(add1(6442450941))" "6442450943";
      t "add1_test5" "add1(add1(-6442450941))" "-6442450939";
      t "add1_test6" "add1(add1(4611686018427387900))" "4611686018427387902";
      t "add1_test7" "add1((let x = 1, y = 2 in x + y))" "4";
      t "add1_test8" "add1((if (let x = 1, y = 2 in x < y): 3 else: 4))" "4";
      t "sub1_test1" "sub1(4611686018427387900)" "4611686018427387899";
      t "sub1_test2" "sub1(sub1(4))" "2";
      t "sub1_test3" "sub1(sub1(1))" "-1";
      t "sub1_test4" "sub1(sub1(6442450941))" "6442450939";
      t "sub1_test5" "sub1(sub1(-6442450941))" "-6442450943";
      t "sub1_test6" "sub1((let x = 1, y = 2 in x + y))" "2";
      t "sdd1_test7" "sub1((if (let x = 1, y = 2 in x < y): 3 else: 4))" "2";
      t "sub1_add1_test1" "sub1(add1(1))" "1";
      t "sub1_add1_test2" "add1(sub1(1))" "1";
      t "print1_test1" "print(1)" "1\n1";
      t "print1_test2" "print(add1(print(1)))" "1\n2\n2";
      t "isBool_test1" "isbool(1)" "false";
      t "isBool_test2" "isbool(false)" "true";
      t "isBool_test3" "isbool(true)" "true";
      t "isBool_test4" "isbool(0)" "false";
      t "isBool_test5" "isbool(-5)" "false";
      t "isBool_test6" "isbool((let x = 1, y = 2 in x + y))" "false";
      t "isBool_test7" "isbool((let x = 1, y = 2 in x < y))" "true";
      t "isBool_test8" "isbool((if (let x = 1, y = 2 in x < y): 3 else: 4))" "false";
      t "isBool_test9" "isbool((if (let x = 1, y = 2 in x < y): true else: false))" "true";
      t "isNum_test1" "isnum(1)" "true";
      t "isNum_test2" "isnum(false)" "false";
      t "isNum_test3" "isnum(true)" "false";
      t "isNum_test4" "isnum(0)" "true";
      t "isNum_test5" "isnum(-5)" "true"; (* Add some more integration tests with prim2 *)
      t "isNum_test6" "isnum((let x = 1, y = 2 in x + y))" "true";
      t "isNum_test7" "isnum((let x = 1, y = 2 in x < y))" "false";
      t "isNum_test8" "isnum((if (let x = 1, y = 2 in x < y): 3 else: 4))" "true";
      t "isNum_test9" "isnum((if (let x = 1, y = 2 in x < y): true else: false))" "false";
      t "not_test1" "!(true)" "false";
      t "not_test2" "!(false)" "true";
      t "not_test3" "!((let x = 1, y = 2 in x < y))" "false";
      t "not_test4" "!((let x = 1, y = 2 in x <= y))" "false";
      t "not_test5" "!((let x = 1, y = 2 in x > y))" "true";
      t "not_test6" "!((let x = 1, y = 2 in x >= y))" "true";
      t "not_test7" "!((let x = 1, y = 2 in x == y))" "true";
      t "not_test8" "!((let x = 2, y = 2 in x == y))" "false";
      t "not_test9" "!((if (let x = 1, y = 2 in x < y): true else: false))" "false";
      t "not_test10" "!((if (let x = 1, y = 2 in x < y): false else: true))" "true";
      te "add1_error1" "add1(false)" "arithmetic expected a number";
      te "add1_error2" "add1(true)" "arithmetic expected a number";
      te "add1_error3" "add1(add1(true))" "arithmetic expected a number";
      te "sub1_error1" "sub1(false)" "arithmetic expected a number";
      te "sub1_error2" "sub1(true)" "arithmetic expected a number";
      te "sub1_error3" "sub1(sub1(true))" "arithmetic expected a number";
      te "not1_error1" "!(-1)" "logic expected a boolean";
      te "not1_error2" "!(0)" "logic expected a boolean";
      te "not1_error3" "!(add1(0))" "logic expected a boolean";
      te "add1_overflow1" ("add1(" ^ string_of_int signed_int63_max ^ ")") "overflow";
      te "add1_overflow2" "add1(4611686018427387903)" "overflowed with value";
      te "add1_overflow3" "add1(-4611686018427387905)" "Integer overflow";
      te "add1_overflow4" "add1(add1(4611686018427387902))" "overflowed with value";
      te "sub1_overflow1" ("sub1(" ^ string_of_int signed_int63_min ^ ")") "overflow";
      te "sub1_overflow2" "sub1(-4611686018427387904)" "overflowed with value";
      te "sub1_overflow3" "sub1(4611686018427387904)" "Integer overflow";
      te "sub1_overflow4" "sub1(sub1(-4611686018427387903))" "overflowed with value";
      
  ]
;;

let prim2_tests = 
  [
    t "prim2_add_test1" "(5+5)" "10";
    t "prim2_add_test2" "(5+5) + 5" "15";
    t "prim2_add_test3" "3+(5+5)" "13";
    t "prim2_add_test4" "3+((6+7)+5)" "21";
    t "prim2_add_test5" "3+-5" "-2";
    t "prim2_add_test6" "1+4611686018427387900" "4611686018427387901";
    t "prim2_add_test7" "4611686018427387900 + 1" "4611686018427387901";
    t "plus_not_overflow1" ((string_of_int signed_int63_max) ^ "+ 0") (string_of_int signed_int63_max);
    t "plus_not_overflow2" ((string_of_int signed_int63_min) ^ "+ 0") (string_of_int signed_int63_min);
    t "prim2_minus_test1" "5 - 2" "3";
    t "prim2_minus_test2" "(10 - 2) - 2" "6";
    t "prim2_minus_test3" "10 - (10 - 2)" "2";
    t "prim2_minus_test4" "10-((20 - 10) - 2)" "2";
    t "prim2_minus_test5" "1 - 4611686018427387901" "-4611686018427387900";
    t "prim2_minus_test6" "4611686018427387901 - 1" "4611686018427387900";
    t "minus_not_overflow1" ((string_of_int signed_int63_max) ^ "- 0") (string_of_int signed_int63_max);
    t "minus_not_overflow2" ((string_of_int signed_int63_min) ^ "- 0") (string_of_int signed_int63_min);
    t "minus_not_overflow3" ("-1 - " ^ (string_of_int signed_int63_max)) "-4611686018427387904";
    t "prim2_times_test1" "2 * 2" "4";
    t "prim2_times_test2" "2*(3*4)" "24";
    t "prim2_times_test3" "(3*4) * 3" "36";
    t "prim2_times_test4" "((2*3) * 4) * 3" "72";
    t "prim2_times_test5" "2*46116860184273879" "92233720368547758";
    t "prim2_times_test6" "46116860184273879*2" "92233720368547758";
    t "times_not_overflow1" ((string_of_int signed_int63_max) ^ "* 1") (string_of_int signed_int63_max);
    t "times_not_overflow2" ((string_of_int signed_int63_min) ^ "* 1") (string_of_int signed_int63_min);
    t "prim2_mix_test1" "(6+4)-(2+3)" "5";
    t "prim2_mix_test2" "(6 - 4)+(2 - 3)" "1";
    t "prim2_mix_test3" "(6*4)+(2*-3)" "18";
    t "prim2_mix_test4" "(6+4)*(2+-3)" "-10";
    t "prim2_mix_test5" "(6 - 4)*(2 - -3)" "10";
    t "prim2_mix_test6" "(7*2)-(-1*-3)" "11";
    t "and_test1" "true && false" "false";
    t "and_test2" "false && true" "false";
    t "and_test3" "false && false" "false";
    t "and_test4" "true && true" "true";
    t "and_test5" "false && 1" "false";
    t "and_test6" "false && -1" "false";
    t "and_test7" "false && true && -1" "false";
    t "and_test8" "true && false && -1" "false";
    t "and_test9" "true && false && true && -1" "false";
    t "and_test10" "true && true && false && -1" "false";
    t "and_test11" "false && (let x=5 in 13 && 13)" "false";
    t "and_test12" "false && (let x=(1 && 1) in 1 && 1)" "false";
    t "and_test13" "false && (if (1 && 1): 1 else: 0)" "false";
    t "and_test14" "false && 100 && false" "false";
    t "and_test15" "false && (1 + false)" "false";
    t "and_test16" "(6 < 5) && (1 + false)" "false";
    t "or_test1" "true || false" "true";
    t "or_test2" "false || true" "true";
    t "or_test3" "false || false" "false";
    t "or_test4" "true || true" "true";
    t "or_test5" "true || 1" "true";
    t "or_test6" "true || -1" "true";
    t "or_test7" "false || true || -1" "true";
    t "or_test8" "true || false || -1" "true";
    t "or_test9" "false || false || true || -1" "true";
    t "or_test10" "true || false || false || -1" "true";
    t "or_test11" "true || (let x=10 in 1 && 1)" "true";
    t "or_test12" "true || (let x=(1 && 1) in 1 && 1)" "true";
    t "or_test13" "true || (if (1 && 1): 1 else: 0)" "true";
    t "or_test14" "true || (1 + true)" "true";
    t "or_test15" "(4 < 5) || (1 + false)" "true";
    t "greater_test1" "5 > 4" "true";
    t "greater_test2" "3 > 4" "false";
    t "greater_test3" "4 > 4" "false";
    t "greater_test4" "-4 > -3" "false";
    t "greater_test5" "-2 > -3" "true";
    t "greater_test6" "-3 > -3" "false";
    t "greater_test7" "4611686018427387901 > 4611686018427387901" "false";
    t "greater_test8" "4611686018427387902 > 4611686018427387901" "true";
    t "greater_test9" "4611686018427387900 > 4611686018427387901" "false";
    t "greater_test10" "3611686018427387901 > 4611686018427387901" "false";
    t "greatereq_test1" "5 >= 4" "true";
    t "greatereq_test2" "3 >= 4" "false";
    t "greatereq_test3" "4 >= 4" "true";
    t "greatereq_test4" "-4 >= -3" "false";
    t "greatereq_test5" "-2 >= -3" "true";
    t "greatereq_test6" "-3 >= -3" "true";
    t "greatereq_test7" "4611686018427387901 >= 4611686018427387901" "true";
    t "greatereq_test8" "4611686018427387902 >= 4611686018427387901" "true";
    t "greatereq_test9" "4611686018427387900 >= 4611686018427387901" "false";
    t "greatereq_test10" "3611686018427387901 >= 4611686018427387901" "false";
    t "less_test1" "5 < 4" "false";
    t "less_test2" "3 < 4" "true";
    t "less_test3" "4 < 4" "false";
    t "less_test4" "-4 < -3" "true";
    t "less_test5" "-2 < -3" "false";
    t "less_test6" "-3 < -3" "false";
    t "less_test7" "4611686018427387901 < 4611686018427387901" "false";
    t "less_test8" "4611686018427387902 < 4611686018427387901" "false";
    t "less_test9" "4611686018427387900 < 4611686018427387901" "true";
    t "less_test10" "3611686018427387901 < 4611686018427387901" "true";
    t "lesseq_test1" "5 <= 4" "false";
    t "lesseq_test2" "3 <= 4" "true";
    t "lesseq_test3" "4 <= 4" "true";
    t "lesseq_test4" "-4 <= -3" "true";
    t "lesseq_test5" "-2 <= -3" "false";
    t "lesseq_test6" "-3 <= -3" "true";
    t "lesseq_test7" "4611686018427387901 <= 4611686018427387901" "true";
    t "lesseq_test8" "4611686018427387902 <= 4611686018427387901" "false";
    t "lesseq_test9" "4611686018427387900 <= 4611686018427387901" "true";
    t "lesseq_test10" "3611686018427387901 <= 4611686018427387901" "true";
    t "eq_test1" "-1 == -1" "true";
    t "eq_test2" "1 == -1" "false";
    t "eq_test3" "55 == -55" "false";
    t "eq_test4" "6123 == 6123" "true";
    t "eq_test5" "false == false" "true";
    t "eq_test6" "false == true" "false";
    t "eq_test7" "true == false" "false";
    t "eq_test8" "true == true" "true";
    t "eq_test9" "4611686018427387901 == 4611686018427387901" "true";
    t "eq_test10" "4611686018427387900 == 4611686018427387901" "false";
    t "eq_test11" "3611686018427387901 == 4611686018427387901" "false";
    te "add_error1" "1 + false" "arithmetic expected a number";
    te "add_error2" "false + 1" "arithmetic expected a number";
    te "add_error3" "true + 1" "arithmetic expected a number";
    te "add_error4" "1 + true" "arithmetic expected a number";
    te "add_error5" "true + true" "arithmetic expected a number";
    te "add_error6" "false + true" "arithmetic expected a number";
    te "add_error7" "true + false" "arithmetic expected a number";
    te "add_error8" "false + false" "arithmetic expected a number";
    te "minus_error1" "1 - false" "arithmetic expected a number";
    te "minus_error2" "false - 1" "arithmetic expected a number";
    te "minus_error3" "true - true" "arithmetic expected a number";
    te "minus_error4" "1 - true" "arithmetic expected a number";
    te "minus_error5" "true - true" "arithmetic expected a number";
    te "minus_error6" "false - true" "arithmetic expected a number";
    te "minus_error7" "true - false" "arithmetic expected a number";
    te "minus_error8" "false - false" "arithmetic expected a number";
    te "times_error1" "1 * false" "arithmetic expected a number";
    te "times_error2" "false * 1" "arithmetic expected a number";
    te "times_error3" "true * false" "arithmetic expected a number";
    te "times_error4" "1 * true" "arithmetic expected a number";
    te "times_error5" "true * true" "arithmetic expected a number";
    te "times_error6" "false * true" "arithmetic expected a number";
    te "times_error7" "true * false" "arithmetic expected a number";
    te "times_error8" "false * false" "arithmetic expected a number";
    te "and_error1" "1 && false" "logic expected a boolean";
    te "and_error3" "1 && 213" "logic expected a boolean";
    te "and_error5" "-1 && false" "logic expected a boolean";
    te "and_error6" "1 && true" "logic expected a boolean";
    te "and_error7" "true && 1" "logic expected a boolean";
    te "and_error8" "true && -1" "logic expected a boolean";
    te "and_error9" "-1 && true" "logic expected a boolean";
    te "and_error8" "true && true && -1" "logic expected a boolean";
    te "and_error9" "true && true && true && -1" "logic expected a boolean";
    te "or_error1" "1 || false" "logic expected a boolean";
    te "or_error2" "false || 1" "logic expected a boolean";
    te "or_error3" "1 || 213" "logic expected a boolean";
    te "or_error4" "false || -1" "logic expected a boolean";
    te "or_error5" "-1 || false" "logic expected a boolean";
    te "or_error6" "1 || true" "logic expected a boolean";
    te "or_error7" "-1 || true" "logic expected a boolean";
    te "greater_error1" "1 > false" "comparison expected a number";
    te "greater_error2" "false > 1" "comparison expected a number";
    te "greater_error3" "true > false" "comparison expected a number";
    te "greater_error4" "false > true" "comparison expected a number";
    te "greater_error5" "false > false" "comparison expected a number";
    te "greater_error6" "true > true" "comparison expected a number";
    te "greater_error7" "1 > true" "comparison expected a number";
    te "greater_error8" "true > 1" "comparison expected a number";
    te "greatereq_error1" "1 >= false" "comparison expected a number";
    te "greatereq_error2" "false >= 1" "comparison expected a number";
    te "greatereq_error3" "true >= false" "comparison expected a number";
    te "greatereq_error4" "false >= true" "comparison expected a number";
    te "greatereq_error5" "false >= false" "comparison expected a number";
    te "greatereq_error6" "true >= true" "comparison expected a number";
    te "greatereq_error7" "1 >= true" "comparison expected a number";
    te "greatereq_error8" "true >= 1" "comparison expected a number";
    te "less_error1" "1 < false" "comparison expected a number";
    te "less_error2" "false < 1" "comparison expected a number";
    te "less_error3" "true < false" "comparison expected a number";
    te "less_error4" "false < true" "comparison expected a number";
    te "less_error5" "false < false" "comparison expected a number";
    te "less_error6" "true < true" "comparison expected a number";
    te "less_error7" "1 < true" "comparison expected a number";
    te "less_error8" "true < 1" "comparison expected a number";
    te "lesseq_error1" "1 <= false" "comparison expected a number";
    te "lesseq_error2" "false <= 1" "comparison expected a number";
    te "lesseq_error3" "true <= false" "comparison expected a number";
    te "lesseq_error4" "false <= true" "comparison expected a number";
    te "lesseq_error5" "false <= false" "comparison expected a number";
    te "lesseq_error6" "true <= true" "comparison expected a number";
    te "lesseq_error7" "1 <= true" "comparison expected a number";
    te "lesseq_error8" "true <= 1" "comparison expected a number";
    te "plus_overflow1" ((string_of_int signed_int63_max) ^ "+ 1") "overflow";
    te "plus_overflow2" ((string_of_int signed_int63_max) ^ "+ 100") "overflow";
    te "plus_overflow3" ((string_of_int signed_int63_min) ^ "+ -1") "overflow";
    te "plus_overflow4" ("1 + " ^ (string_of_int signed_int63_max)) "overflow";
    te "plus_overflow5" ("100 + " ^ (string_of_int signed_int63_max)) "overflow";
    te "plus_overflow6" ("-1 + " ^ (string_of_int signed_int63_min)) "overflow";
    te "plus_overflow7" "4611686018427387903 + 1" "overflowed with value";
    te "plus_overflow8" "2611686018427387903 + 2611686018427387903" "overflowed with value";
    te "plus_overflow9" "(-4611686018427387904) + (-1)" "overflowed with value";
    te "minus_overflow1" ((string_of_int signed_int63_min) ^ "- 1") "overflow";
    te "minus_overflow2" ((string_of_int signed_int63_min) ^ "- 100") "overflow";
    te "minus_overflow3" ((string_of_int signed_int63_max) ^ "- -1") "overflow";
    te "minus_overflow4" ("-2 - " ^ (string_of_int signed_int63_max)) "overflow";
    te "minus_overflow5" ("-100 - " ^ (string_of_int signed_int63_max)) "overflow";
    te "minus_overflow6" ("1 - " ^ (string_of_int signed_int63_min)) "overflow";
    te "minus_overflow7" "-4611686018427387904 - 1" "overflowed with value";
    te "minus_overflow8" "-2611686018427387904 - 2611686018427387904" "overflowed with value";
    te "minus_overflow9" "4611686018427387903 - (-1)" "overflowed with value";
    te "times_overflow1" ((string_of_int signed_int63_max) ^ "* 2") "overflow";
    te "times_overflow2" ((string_of_int signed_int63_max) ^ "* 100") "overflow";
    te "times_overflow3" ((string_of_int signed_int63_min) ^ "* -2") "overflow";
    te "times_overflow4" ((string_of_int signed_int63_min) ^ "* -100") "overflow";
    te "times_overflow5" ((string_of_int signed_int63_max) ^ "* -2") "overflow";
    te "times_overflow6" ((string_of_int signed_int63_max) ^ "* -100") "overflow";
    te "times_overflow7" ((string_of_int signed_int63_min) ^ "* 2") "overflow";
    te "times_overflow8" ((string_of_int signed_int63_min) ^ "* 100") "overflow";
    te "times_overflow9" ("2 * " ^ (string_of_int signed_int63_max)) "overflow";
    te "times_overflow10" ("100 * " ^ (string_of_int signed_int63_max)) "overflow";
    te "times_overflow11" ("-2 * " ^ (string_of_int signed_int63_min)) "overflow";
    te "times_overflow12" ("-100 *" ^ (string_of_int signed_int63_min)) "overflow";
    te "times_overflow13" ("-2 * " ^ (string_of_int signed_int63_max)) "overflow";
    te "times_overflow14" ("-100 * " ^ (string_of_int signed_int63_max)) "overflow";
    te "times_overflow15" ("2 * " ^ (string_of_int signed_int63_min)) "overflow";
    te "times_overflow16" ("100 * " ^ (string_of_int signed_int63_min)) "overflow";
    te "times_overflow17" "461168601842738790 * 11" "overflowed with value";
    te "times_overflow18" "461168601842738790 * -11" "overflowed with value";
    te "times_overflow19" "46116860184273879 * 110" "overflowed with value";
    te "times_overflow20" "46116860184273879 * -110" "overflowed with value";
    te "times_overflow21" "-461168601842738790 * -11" "overflowed with value";
    te "times_overflow22" "-46116860184273879 * -110" "overflowed with value";
  ]
;;

let if_tests = [
  t "if_test1" "if true: 1 else: 0" "1";
  t "if_test2" "if false: 1 else: 0" "0";
  t "if_test3" "if 5 < 6: 1 else: 0" "1";
  t "if_test4" "if !(false): 1 else: 0" "1";
  t "if_test5" "if !(true): 1 else: 0" "0";
  t "if_test6" "if !(true): true else: false" "false";
  t "if_test7" "if (if false: true else: false): true else: false" "false";
  t "if_test8" "if isnum(5): 1 else: 0" "1";
  t "if_test9" "if isbool(false): 1 else: 0" "1";
  t "if_test10" "if isnum(false): 1 else: 0" "0";
  t "if_test11" "if isbool(1): 1 else: 0" "0";
  t "if_test12" "if (let x = 1, y = 2 in x < y): 3 else: 4" "3"; (* if containing let in cond *) 
  t "if_test13" "if (let x = 1, y = 2 in x < y): (let x = 2, y = 2 in x == y) else: (let x = 1, y = 2 in x > y)" "true"; (* if containing let in cond, then, else *) 
  te "if_error1" "if 1: 1 else: false" "if expected a boolean";
  te "if_error2" "if add1(1): 1 else: false" "if expected a boolean";
  te "if_error3" "if 1 + 1: 1 else: false" "if expected a boolean";
]

let let_tests = [
    t "let_test1" "(let x = 10 in x)" "10";
    t "let_test2" "(let x=10,y=5 in x)" "10";
    t "let_test3" "(let x=10,y=5 in y)" "5";
    t "let_test4" "(let x=add1(10),y=5 in x)" "11";
    t "let_test5" "(let x=add1(69),y=add1(x) in y)" "71";
    t "let_test6" "(let x=add1(10),y=add1(x),z=add1(y) in z)" "13";
    t "let_test7" "(let x=add1(10),y=add1(x),z=add1(y) in x)" "11";
    t "let_test8" "(let x=add1((let x=add1(10),y=5 in x)),y=5 in x)" "12";
    t "let_test9" "(let x=(add1 (let x=add1(10),y=5 in x)),y=(let z=add1(x) in z) in y)" "13";
    t "let_test10" "(let x=10,y=(let x=5 in x) in y)" "5";
    t "let_test11" "(let x=add1(10),y=add1(x),z=add1(y) in z)" "13";
    t "let_test12" "(let x=2*2,y=x - 1,z=y+2 in z * 2)" "10";
    t "let_test13" "(let x=(2+3)*2,y=10-(x+1) in y)" "-1";
    t "let_test14" "(let x=(2+3)*(1+1),y=(5+5)-(x+1) in y)" "-1";
    t "let_test15" "(let x=(2+3)*(1+1),y=(if true: x else: 0) in y)" "10";
    t "let_test16" "(let x = 10, y = (if x: 1 else: 3) in y)" "1";
    t "let_test17" "(let x = -4 in (let x = x + 1 in x))" "-3";
    t "let_test18" "(let x = (let x = 1, y = 2 in x + y) in x)" "3"; (* test that body expression uses env of its scope. *) 
    t "let_test19" "(let x = 1, y = 2 in (let x = 3, y = 4 in x + y))" "7"; (* test that inner bindings shadow outer bindings. *) 
    t "let_test20" "(let x = 1 in (let x = 2 in x))" "2";
    t "let_test21" "(let x = 1 in (let y = 2 in x + y))" "3";
    t "let_test22" "(let x = (if (let x = 1, y = 2 in y > x): 3 else: 4), y = 5 in x + y" "8"; (* let bound if expression *)
    t "let_test23" "let x = (let x = 1 in x + 1), y = (let z = 2 in x + z), z = (if x > y: 5 else: 6) in x + y + z" "10"; (* complex let expression *)
]


let let_and_print_tests = [
  t "let_print_test1" "(let x=10, y=5 in print(add1(print(add1(x)))))" "11\n12\n12";
  t "let_print_test2" "(let x=10, y=5 in print(add1(print(add1(y)))))" "6\n7\n7";
  t "let_print_test3" "(let x=10, y=5, z=1, a=11, b=7, c=2, d=9 in print(x + y + z + a + b + c + d + add1(print(add1(y)))))" "6\n52\n52";
  t "let_print_test4" "(let y=5, z=1, a=11, b=7, c=2, d=9 in print(y + z + a + b + c + d + add1(print(add1(y)))))" "6\n42\n42";
]

let int_overflow_tests =[
  (* te "int_overflow1" (string_of_int (signed_int63_max + 2)) "Integer overflow"; *)
  te "int_overflow1" "4611686018427387904" "Integer overflow";
  te "int_overflow2" "-4611686018427387905" "Integer overflow";
]

let extra_tests = 
  [ ta "forty_one_run_anf" (tag forty_one_a) "41";
           t "forty_one" forty_one "41";
           te "unbound_test1" "x" "Compile.BindingError(\"Variable x not bound at col: 0 line: 1\")";
           te "unbound_test2" "x+2"
             "Compile.BindingError(\"Variable x not bound at col: 0 line: 1\")";
           te "unbound_test3" "3+x"
             "Compile.BindingError(\"Variable x not bound at col: 2 line: 1\")";
           te "same_ids_test1" "(let x=add1(69),x=add1(x) in x)"
             "Compile.BindingError(\"Binding name x bound twice in let at col: 16 line: 1\")";
           t "order_of_operations1" "1 - 2*3" "-3";
           (* This is weird. PEMDAS says it should be -5, but answer is -3*)
           te "syntax_error2" "(1+2) (1+3)"
             "Runner.ParseError(\"Parse error at line 1, col 7: token `(`\")";
           t "order_of_operations2" "5 - 2 - 3" "0";
  t "forty" forty "40";
  t "fals" fals "false";
  t "tru" tru "true";
 
 
 ]

let suite =
  "suite"
  >::: 
  prim1_tests @ prim2_tests @ if_tests @ tag_test_list @ rename_test_list @ let_and_print_tests
    @ int_overflow_tests
;;


(* input_file_test_suite () will run all the tests in the subdirectories of input/ *)
let () =
  run_test_tt_main ("all_tests">:::[suite; input_file_test_suite ()])
;;
