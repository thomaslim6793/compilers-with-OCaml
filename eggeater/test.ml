open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Errors

let t name program input expected = name>::test_run ~args:[] ~std_input:input program name expected;;
let ta name program input expected = name>::test_run_anf ~args:[] ~std_input:input program name expected;;
let tgc name heap_size program input expected = name>::test_run ~args:[string_of_int heap_size] ~std_input:input program name expected;;
let tvg name program input expected = name>::test_run_valgrind ~args:[] ~std_input:input program name expected;;
let tvgc name heap_size program input expected = name>::test_run_valgrind ~args:[string_of_int heap_size] ~std_input:input program name expected;;
let terr name program input expected = name>::test_err ~args:[] ~std_input:input program name expected;;
let tgcerr name heap_size program input expected = name>::test_err ~args:[string_of_int heap_size] ~std_input:input program name expected;;
let tanf name program input expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;
let tsa name program expected =
  name
  >:: fun _ ->
  assert_equal expected
    (naive_stack_allocation (atag (anf (tag (parse_string "stack_test" program)))))
;;
let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

let signed_int63_max = 4611686018427387903

let signed_int63_min = -4611686018427387904

let prim1_tests =
  [ t "add1_test1" "add1(1)" "" "2";
    t "add1_test2" "add1(add1(1))" "" "3";
    t "add1_test3" "add1(add1(-1))" "" "1";
    t "add1_test4" "add1(add1(6442450941))"  "" "6442450943";
    t "add1_test5" "add1(add1(-6442450941))" ""  "-6442450939";
    t "add1_test6" "add1(add1(4611686018427387900))" "" "4611686018427387902";
    t "add1_test7" "add1((let x = 1, y = 2 in x + y))" "" "4";
    t "add1_test8" "add1((if (let x = 1, y = 2 in x < y): 3 else: 4))" "" "4";
    t "add1_test9" "add1(add1(add1(1)))" "" "4";
    t "add1_test10" "add1((1,2)[1])" "" "3";
    t "sub1_test1" "sub1(4611686018427387900)" "" "4611686018427387899";
    t "sub1_test2" "sub1(sub1(4))" "" "2";
    t "sub1_test3" "sub1(sub1(1))" "" "-1";
    t "sub1_test4" "sub1(sub1(6442450941))" "" "6442450939";
    t "sub1_test5" "sub1(sub1(-6442450941))" "" "-6442450943";
    t "sub1_test6" "sub1((let x = 1, y = 2 in x + y))" "" "2";
    t "sub1_test7" "sub1((if (let x = 1, y = 2 in x < y): 3 else: 4))" "" "2";
    t "sub1_test8" "sub1((1,5)[1])" "" "4";
    t "sub1_add1_test1" "sub1(add1(1))" "" "1";
    t "sub1_add1_test2" "add1(sub1(1))" "" "1";
    t "print1_test1" "print(1)" "" "1\n1";
    t "print1_test2" "print(add1(print(1)))" "" "1\n2\n2";
    t "isBool_test1" "isbool(1)" "" "false";
    t "isBool_test2" "isbool(false)" "" "true";
    t "isBool_test3" "isbool(true)" "" "true";
    t "isBool_test4" "isbool(0)" "" "false";
    t "isBool_test5" "isbool(-5)" "" "false";
    t "isBool_test6" "isbool((let x = 1, y = 2 in x + y))" "" "false";
    t "isBool_test7" "isbool((let x = 1, y = 2 in x < y))" "" "true";
    t "isBool_test8" "isbool((if (let x = 1, y = 2 in x < y): 3 else: 4))" "" "false";
    t "isBool_test9" "isbool((if (let x = 1, y = 2 in x < y): true else: false))" "" "true";
    t "isBool_test10" "isbool((1,false))" "" "false";
    t "isNum_test1" "isnum(1)" "" "true";
    t "isNum_test2" "isnum(false)" "" "false";
    t "isNum_test3" "isnum(true)" "" "false";
    t "isNum_test4" "isnum(0)" "" "true";
    t "isNum_test5" "isnum(-5)" "" "true";
    t "isNum_test6" "isnum((let x = 1, y = 2 in x + y))" "" "true";
    t "isNum_test7" "isnum((let x = 1, y = 2 in x < y))" "" "false";
    t "isNum_test8" "isnum((if (let x = 1, y = 2 in x < y): 3 else: 4))" "" "true";
    t "isNum_test9" "isnum((if (let x = 1, y = 2 in x < y): true else: false))" "" "false";
    t "isNum_test10" "isnum((1, 2))" "" "false";
    t "isTuple_test1" "istuple(1)" "" "false";
    t "isTuple_test2" "istuple(false)" "" "false";
    t "isTuple_test3" "istuple((1,))" "" "true";
    t "isTuple_test4" "istuple((1))" "" "false";
    t "not_test1" "!(true)" "" "false";
    t "not_test2" "!(false)" "" "true";
    t "not_test3" "!((let x = 1, y = 2 in x < y))" "" "false";
    t "not_test4" "!((let x = 1, y = 2 in x <= y))" "" "false";
    t "not_test5" "!((let x = 1, y = 2 in x > y))" "" "true";
    t "not_test6" "!((let x = 1, y = 2 in x >= y))" "" "true";
    t "not_test7" "!((let x = 1, y = 2 in x == y))" "" "true";
    t "not_test8" "!((let x = 2, y = 2 in x == y))" "" "false";
    t "not_test9" "!((if (let x = 1, y = 2 in x < y): true else: false))" "" "false";
    t "not_test10" "!((if (let x = 1, y = 2 in x < y): false else: true))" "" "true";
    t "not_test11" "!((false, 1)[0])" "" "true";
    terr"add1_error1" "add1(false)"  "" "arithmetic expected a number";
    terr "add1_error2" "add1(true)"  "" "arithmetic expected a number";
    terr "add1_error3" "add1(add1(true))"  "" "arithmetic expected a number";
    terr "add1_error4" "add1((1,2))"  "" "arithmetic expected a number";
    terr "sub1_error1" "sub1(false)" "" "arithmetic expected a number";
    terr "sub1_error2" "sub1(true)" "" "arithmetic expected a number";
    terr "sub1_error3" "sub1(sub1(true))" "" "arithmetic expected a number";
    terr "sub1_error4" "sub1((1, 2))" "" "arithmetic expected a number";
    terr "not1_error1" "!(-1)" "" "logic expected a boolean";
    terr "not1_error2" "!(0)" "" "logic expected a boolean";
    terr "not1_error3" "!(add1(0))" "" "logic expected a boolean";
    terr "not1_error4" "!((1,2))" "" "logic expected a boolean";
    terr "add1_overflow1" ("add1(" ^ string_of_int signed_int63_max ^ ")") "" "overflow";
    terr "add1_overflow2" "add1(4611686018427387903)" "" "overflowed with value";
    terr "add1_overflow3" "add1(-4611686018427387905)" "" "The number literal";
    terr "add1_overflow4" "add1(add1(4611686018427387902))" "" "overflowed with value";
    terr "sub1_overflow1" ("sub1(" ^ string_of_int signed_int63_min ^ ")") "" "overflow";
    terr "sub1_overflow2" "sub1(-4611686018427387904)" "" "overflowed with value";
    terr "sub1_overflow3" "sub1(4611686018427387904)" "" "The number literal";
    terr "sub1_overflow4" "sub1(sub1(-4611686018427387903))" "" "overflowed with value" ]
;;

let prim2_tests =
  [ t "prim2_add_test1" "(5+5)" "" "10";
    t "prim2_add_test2" "(5+5) + 5" "" "15";
    t "prim2_add_test3" "3+(5+5)" "" "13";
    t "prim2_add_test4" "3+((6+7)+5)" "" "21";
    t "prim2_add_test5" "3+-5" "" "-2";
    t "prim2_add_test6" "1+4611686018427387900" "" "4611686018427387901";
    t "prim2_add_test7" "4611686018427387900 + 1" "" "4611686018427387901";
    t "prim2_add_test8" "((1,5)[1]+5)" "" "10";
    t "prim2_add_test9" "(5+(1,5)[1])" "" "10";
    t "prim2_add_test10" "((5,1)[0]+(1,5)[1])" "" "10";
    t "plus_not_overflow1"
      (string_of_int signed_int63_max ^ "+ 0")
      ""
      (string_of_int signed_int63_max);
    t "plus_not_overflow2"
      (string_of_int signed_int63_min ^ "+ 0")
      ""
      (string_of_int signed_int63_min);
    t "prim2_minus_test1" "5 - 2" "" "3";
    t "prim2_minus_test2" "(10 - 2) - 2" "" "6";
    t "prim2_minus_test3" "10 - (10 - 2)" "" "2";
    t "prim2_minus_test4" "10-((20 - 10) - 2)" "" "2";
    t "prim2_minus_test5" "1 - 4611686018427387901" "" "-4611686018427387900";
    t "prim2_minus_test6" "4611686018427387901 - 1" "" "4611686018427387900";
    t "prim2_minus_test7" "5 - (2, 1)[0]" "" "3";
    t "prim2_minus_test8" "(8, 6, 7, 5)[3] - 2" "" "3";
    t "prim2_minus_test9" "(8, 6, 7, 5)[3] - (2, 1)[0]" "" "3";
    t "minus_not_overflow1"
      (string_of_int signed_int63_max ^ "- 0")
      ""
      (string_of_int signed_int63_max);
    t "minus_not_overflow2"
      (string_of_int signed_int63_min ^ "- 0")
      ""
      (string_of_int signed_int63_min);
    t "minus_not_overflow3" ("-1 - " ^ string_of_int signed_int63_max) "" "-4611686018427387904";
    t "prim2_times_test1" "2 * 2" "" "4";
    t "prim2_times_test2" "2*(3*4)" "" "24";
    t "prim2_times_test3" "(3*4) * 3" "" "36";
    t "prim2_times_test4" "((2*3) * 4) * 3" "" "72";
    t "prim2_times_test5" "2*46116860184273879" "" "92233720368547758";
    t "prim2_times_test6" "46116860184273879*2" "" "92233720368547758";
    t "times_not_overflow1"
      (string_of_int signed_int63_max ^ "* 1")
      ""
      (string_of_int signed_int63_max);
    t "times_not_overflow2"
      (string_of_int signed_int63_min ^ "* 1")
      ""
      (string_of_int signed_int63_min);
    t "prim2_mix_test1" "(6+4)-(2+3)" "" "5";
    t "prim2_mix_test2" "(6 - 4)+(2 - 3)" "" "1";
    t "prim2_mix_test3" "(6*4)+(2*-3)" "" "18";
    t "prim2_mix_test4" "(6+4)*(2+-3)" "" "-10";
    t "prim2_mix_test5" "(6 - 4)*(2 - -3)" "" "10";
    t "prim2_mix_test6" "(7*2)-(-1*-3)" "" "11";
    t "prim2_mix_test7" "(7*(1,2,false)[1])-(-1*-3)" "" "11";
    t "and_test1" "true && false" "" "false";
    t "and_test2" "false && true" "" "false";
    t "and_test3" "false && false" "" "false";
    t "and_test4" "true && true" "" "true";
    t "and_test5" "false && 1" "" "false";
    t "and_test6" "false && -1" "" "false";
    t "and_test7" "false && true && -1" "" "false";
    t "and_test8" "true && false && -1" "" "false";
    t "and_test9" "true && false && true && -1" "" "false";
    t "and_test10" "true && true && false && -1" "" "false";
    t "and_test11" "false && (let x=5 in 13 && 13)" "" "false";
    t "and_test12" "false && (let x=(1 && 1) in 1 && 1)" "" "false";
    t "and_test13" "false && (if (1 && 1): 1 else: 0)" "" "false";
    t "and_test14" "false && 100 && false" "" "false";
    t "and_test15" "false && (1 + false)" "" "false";
    t "and_test16" "(6 < 5) && (1 + false)" "" "false";
    t "and_test17" "let x = (6 < 5) && (1 + false) in x" "" "false";
    t "or_test1" "true || false" "" "true";
    t "or_test2" "false || true" "" "true";
    t "or_test3" "false || false" "" "false";
    t "or_test4" "true || true" "" "true";
    t "or_test5" "true || 1" "" "true";
    t "or_test6" "true || -1" "" "true";
    t "or_test7" "false || true || -1" "" "true";
    t "or_test8" "true || false || -1" "" "true";
    t "or_test9" "false || false || true || -1" "" "true";
    t "or_test10" "true || false || false || -1" "" "true";
    t "or_test11" "true || (let x=10 in 1 && 1)" "" "true";
    t "or_test12" "true || (let x=(1 && 1) in 1 && 1)" "" "true";
    t "or_test13" "true || (if (1 && 1): 1 else: 0)" "" "true";
    t "or_test14" "true || (1 + true)" "" "true";
    t "or_test15" "(4 < 5) || (1 + false)" "" "true";
    t "or_test16" "true || 100 || false" "" "true";
    t "or_test17" "!(true || -1)" "" "false";
    t "or_test18" "let x = (4 < 5) || (1 + false) in x" "" "true";
    t "greater_test1" "5 > 4" "" "true";
    t "greater_test2" "3 > 4" "" "false";
    t "greater_test3" "4 > 4" "" "false";
    t "greater_test4" "-4 > -3" "" "false";
    t "greater_test5" "-2 > -3" "" "true";
    t "greater_test6" "-3 > -3" "" "false";
    t "greater_test7" "4611686018427387901 > 4611686018427387901" "" "false";
    t "greater_test8" "4611686018427387902 > 4611686018427387901" "" "true";
    t "greater_test9" "4611686018427387900 > 4611686018427387901" "" "false";
    t "greater_test10" "3611686018427387901 > 4611686018427387901" "" "false";
    t "greatereq_test1" "5 >= 4" "" "true";
    t "greatereq_test2" "3 >= 4" "" "false";
    t "greatereq_test3" "4 >= 4" "" "true";
    t "greatereq_test4" "-4 >= -3" "" "false";
    t "greatereq_test5" "-2 >= -3" "" "true";
    t "greatereq_test6" "-3 >= -3" "" "true";
    t "greatereq_test7" "4611686018427387901 >= 4611686018427387901" "" "true";
    t "greatereq_test8" "4611686018427387902 >= 4611686018427387901" "" "true";
    t "greatereq_test9" "4611686018427387900 >= 4611686018427387901" "" "false";
    t "greatereq_test10" "3611686018427387901 >= 4611686018427387901" "" "false";
    t "less_test1" "5 < 4" "" "false";
    t "less_test2" "3 < 4" "" "true";
    t "less_test3" "4 < 4" "" "false";
    t "less_test4" "-4 < -3" "" "true";
    t "less_test5" "-2 < -3" "" "false";
    t "less_test6" "-3 < -3" "" "false";
    t "less_test7" "4611686018427387901 < 4611686018427387901" "" "false";
    t "less_test8" "4611686018427387902 < 4611686018427387901" "" "false";
    t "less_test9" "4611686018427387900 < 4611686018427387901" "" "true";
    t "less_test10" "3611686018427387901 < 4611686018427387901" "" "true";
    t "lesseq_test1" "5 <= 4" "" "false";
    t "lesseq_test2" "3 <= 4" "" "true";
    t "lesseq_test3" "4 <= 4" "" "true";
    t "lesseq_test4" "-4 <= -3" "" "true";
    t "lesseq_test5" "-2 <= -3" "" "false";
    t "lesseq_test6" "-3 <= -3" "" "true";
    t "lesseq_test7" "4611686018427387901 <= 4611686018427387901" "" "true";
    t "lesseq_test8" "4611686018427387902 <= 4611686018427387901" "" "false";
    t "lesseq_test9" "4611686018427387900 <= 4611686018427387901" "" "true";
    t "lesseq_test10" "3611686018427387901 <= 4611686018427387901" "" "true";
    t "eq_test1" "-1 == -1" "" "true";
    t "eq_test2" "1 == -1" "" "false";
    t "eq_test3" "55 == -55" "" "false";
    t "eq_test4" "6123 == 6123" "" "true";
    t "eq_test5" "false == false" "" "true";
    t "eq_test6" "false == true" "" "false";
    t "eq_test7" "true == false" "" "false";
    t "eq_test8" "true == true" "" "true";
    t "eq_test9" "4611686018427387901 == 4611686018427387901" "" "true";
    t "eq_test10" "4611686018427387900 == 4611686018427387901" "" "false";
    t "eq_test11" "3611686018427387901 == 4611686018427387901" "" "false";
    t "eq_test12" "let x = (1, 2) in x == x" "" "true";
    t "eq_test13" "let x = (1, 2) in x == (1, 2)" "" "false";
    terr "add_error1" "1 + false" "" "arithmetic expected a number";
    terr "add_error2" "false + 1" "" "arithmetic expected a number";
    terr "add_error3" "true + 1" "" "arithmetic expected a number";
    terr "add_error4" "1 + true" "" "arithmetic expected a number";
    terr "add_error5" "true + true" "" "arithmetic expected a number";
    terr "add_error6" "false + true" "" "arithmetic expected a number";
    terr "add_error7" "true + false" "" "arithmetic expected a number";
    terr "add_error8" "false + false" "" "arithmetic expected a number";
    terr "add_error9" "(1, 2) + 1" "" "arithmetic expected a number";
    terr "add_error10" "1 + (1, 2)" "" "arithmetic expected a number";
    terr "minus_error1" "1 - false" "" "arithmetic expected a number";
    terr "minus_error2" "false - 1" "" "arithmetic expected a number";
    terr "minus_error3" "true - true" "" "arithmetic expected a number";
    terr "minus_error4" "1 - true" "" "arithmetic expected a number";
    terr "minus_error5" "true - true" "" "arithmetic expected a number";
    terr "minus_error6" "false - true" "" "arithmetic expected a number";
    terr "minus_error7" "true - false" "" "arithmetic expected a number";
    terr "minus_error8" "false - false" "" "arithmetic expected a number";
    terr "minus_error9" "(1, 2) - 1" "" "arithmetic expected a number";
    terr "minus_error10" "1 - (1, 2)" "" "arithmetic expected a number";
    terr "times_error1" "1 * false" "" "arithmetic expected a number";
    terr "times_error2" "false * 1" "" "arithmetic expected a number";
    terr "times_error3" "true * false" "" "arithmetic expected a number";
    terr "times_error4" "1 * true" "" "arithmetic expected a number";
    terr "times_error5" "true * true" "" "arithmetic expected a number";
    terr "times_error6" "false * true" "" "arithmetic expected a number";
    terr "times_error7" "true * false" "" "arithmetic expected a number";
    terr "times_error8" "false * false" "" "arithmetic expected a number";
    terr "times_error9" "(1, 2) * 1" "" "arithmetic expected a number";
    terr "times_error10" "1 * (1, 2)" "" "arithmetic expected a number";
    terr "and_error1" "1 && false" "" "logic expected a boolean";
    terr "and_error3" "1 && 213" "" "logic expected a boolean";
    terr "and_error5" "-1 && false" "" "logic expected a boolean";
    terr "and_error6" "1 && true" "" "logic expected a boolean";
    terr "and_error7" "true && 1" "" "logic expected a boolean";
    terr "and_error8" "true && -1" "" "logic expected a boolean";
    terr "and_error9" "-1 && true" "" "logic expected a boolean";
    terr "and_error10" "true && true && -1" "" "logic expected a boolean";
    terr "and_error11" "true && true && true && -1" "" "logic expected a boolean";
    terr "and_error12" "(1, 2) && false" "" "logic expected a boolean";
    terr "and_error13" "true && (1, 2)" "" "logic expected a boolean";
    terr "or_error1" "1 || false" "" "logic expected a boolean";
    terr "or_error2" "false || 1" "" "logic expected a boolean";
    terr "or_error3" "1 || 213" "" "logic expected a boolean";
    terr "or_error4" "false || -1" "" "logic expected a boolean";
    terr "or_error5" "-1 || false" "" "logic expected a boolean";
    terr "or_error6" "1 || true" "" "logic expected a boolean";
    terr "or_error7" "-1 || true" "" "logic expected a boolean";
    terr "or_error8" "(1, 2) || false" "" "logic expected a boolean";
    terr "or_error9" "false || (1, 2)" "" "logic expected a boolean";
    terr "greater_error1" "1 > false" "" "comparison expected a number";
    terr "greater_error2" "false > 1" "" "comparison expected a number";
    terr "greater_error3" "true > false" "" "comparison expected a number";
    terr "greater_error4" "false > true" "" "comparison expected a number";
    terr "greater_error5" "false > false" "" "comparison expected a number";
    terr "greater_error6" "true > true" "" "comparison expected a number";
    terr "greater_error7" "1 > true" "" "comparison expected a number";
    terr "greater_error8" "true > 1" "" "comparison expected a number";
    terr "greater_error9" "(1, 2) > 1" "" "comparison expected a number";
    terr "greater_error10" "1 > (1, 2)" "" "comparison expected a number";
    terr "greatereq_error1" "1 >= false" "" "comparison expected a number";
    terr "greatereq_error2" "false >= 1" "" "comparison expected a number";
    terr "greatereq_error3" "true >= false" "" "comparison expected a number";
    terr "greatereq_error4" "false >= true" "" "comparison expected a number";
    terr "greatereq_error5" "false >= false" "" "comparison expected a number";
    terr "greatereq_error6" "true >= true" "" "comparison expected a number";
    terr "greatereq_error7" "1 >= true" "" "comparison expected a number";
    terr "greatereq_error8" "true >= 1" "" "comparison expected a number";
    terr "greatereq_error9" "(1, 2) >= 1" "" "comparison expected a number";
    terr "greatereq_error10" "1 >= (1, 2)" "" "comparison expected a number";
    terr "less_error1" "1 < false" "" "comparison expected a number";
    terr "less_error2" "false < 1" "" "comparison expected a number";
    terr "less_error3" "true < false" "" "comparison expected a number";
    terr "less_error4" "false < true" "" "comparison expected a number";
    terr "less_error5" "false < false" "" "comparison expected a number";
    terr "less_error6" "true < true" "" "comparison expected a number";
    terr "less_error7" "1 < true" "" "comparison expected a number";
    terr "less_error8" "true < 1" "" "comparison expected a number";
    terr "less_error9" "1 < (1, 2)" "" "comparison expected a number";
    terr "less_error10" "(1, 2) < 1" "" "comparison expected a number";
    terr "lesseq_error1" "1 <= false" "" "comparison expected a number";
    terr "lesseq_error2" "false <= 1" "" "comparison expected a number";
    terr "lesseq_error3" "true <= false" "" "comparison expected a number";
    terr "lesseq_error4" "false <= true" "" "comparison expected a number";
    terr "lesseq_error5" "false <= false" "" "comparison expected a number";
    terr "lesseq_error6" "true <= true" "" "comparison expected a number";
    terr "lesseq_error7" "1 <= true" "" "comparison expected a number";
    terr "lesseq_error8" "true <= 1" "" "comparison expected a number";
    terr "lesseq_error9" "1 <= (1, 2)" "" "comparison expected a number";
    terr "lesseq_error10" "(1, 2) <= 1" "" "comparison expected a number";
    terr "plus_overflow1" (string_of_int signed_int63_max ^ "+ 1")  "" "overflow";
    terr "plus_overflow2" (string_of_int signed_int63_max ^ "+ 100") "" "overflow";
    terr "plus_overflow3" (string_of_int signed_int63_min ^ "+ -1") "" "overflow";
    terr "plus_overflow4" ("1 + " ^ string_of_int signed_int63_max) "" "overflow";
    terr "plus_overflow5" ("100 + " ^ string_of_int signed_int63_max) "" "overflow";
    terr "plus_overflow6" ("-1 + " ^ string_of_int signed_int63_min) "" "overflow";
    terr "plus_overflow7" "4611686018427387903 + 1" "" "overflowed with value";
    terr "plus_overflow8" "2611686018427387903 + 2611686018427387903" "" "overflowed with value";
    terr "plus_overflow9" "(-4611686018427387904) + (-1)" "" "overflowed with value";
    terr "minus_overflow1" (string_of_int signed_int63_min ^ "- 1") "" "overflow";
    terr "minus_overflow2" (string_of_int signed_int63_min ^ "- 100") "" "overflow";
    terr "minus_overflow3" (string_of_int signed_int63_max ^ "- -1") "" "overflow";
    terr "minus_overflow4" ("-2 - " ^ string_of_int signed_int63_max) "" "overflow";
    terr "minus_overflow5" ("-100 - " ^ string_of_int signed_int63_max) "" "overflow";
    terr "minus_overflow6" ("1 - " ^ string_of_int signed_int63_min) "" "overflow";
    terr "minus_overflow7" "-4611686018427387904 - 1" "" "overflowed with value";
    terr "minus_overflow8" "-2611686018427387904 - 2611686018427387904" "" "overflowed with value";
    terr "minus_overflow9" "4611686018427387903 - (-1)" "" "overflowed with value";
    terr "times_overflow1" (string_of_int signed_int63_max ^ "* 2") "" "overflow";
    terr "times_overflow2" (string_of_int signed_int63_max ^ "* 100") "" "overflow";
    terr "times_overflow3" (string_of_int signed_int63_min ^ "* -2") "" "overflow";
    terr "times_overflow4" (string_of_int signed_int63_min ^ "* -100") "" "overflow";
    terr "times_overflow5" (string_of_int signed_int63_max ^ "* -2") "" "overflow";
    terr "times_overflow6" (string_of_int signed_int63_max ^ "* -100") "" "overflow";
    terr "times_overflow7" (string_of_int signed_int63_min ^ "* 2") "" "overflow";
    terr "times_overflow8" (string_of_int signed_int63_min ^ "* 100") "" "overflow";
    terr "times_overflow9" ("2 * " ^ string_of_int signed_int63_max) "" "overflow";
    terr "times_overflow10" ("100 * " ^ string_of_int signed_int63_max) "" "overflow";
    terr "times_overflow11" ("-2 * " ^ string_of_int signed_int63_min) "" "overflow";
    terr "times_overflow12" ("-100 *" ^ string_of_int signed_int63_min) "" "overflow";
    terr "times_overflow13" ("-2 * " ^ string_of_int signed_int63_max) "" "overflow";
    terr "times_overflow14" ("-100 * " ^ string_of_int signed_int63_max) "" "overflow";
    terr "times_overflow15" ("2 * " ^ string_of_int signed_int63_min) "" "overflow";
    terr "times_overflow16" ("100 * " ^ string_of_int signed_int63_min) "" "overflow";
    terr "times_overflow17" "461168601842738790 * 11" "" "overflowed with value";
    terr "times_overflow18" "461168601842738790 * -11" "" "overflowed with value";
    terr "times_overflow19" "46116860184273879 * 110" "" "overflowed with value";
    terr "times_overflow20" "46116860184273879 * -110" "" "overflowed with value";
    terr "times_overflow21" "-461168601842738790 * -11" "" "overflowed with value";
    terr "times_overflow22" "-46116860184273879 * -110" "" "overflowed with value" ]
;;

let if_tests =
  [ t "if_test1" "if true: 1 else: 0" "" "1";
    t "if_test2" "if false: 1 else: 0" "" "0";
    t "if_test3" "if 5 < 6: 1 else: 0" "" "1";
    t "if_test4" "if !(false): 1 else: 0" "" "1";
    t "if_test5" "if !(true): 1 else: 0" "" "0";
    t "if_test6" "if !(true): true else: false" "" "false";
    t "if_test7" "if (if false: true else: false): true else: false" "" "false";
    t "if_test8" "if isnum(5): 1 else: 0" "" "1";
    t "if_test9" "if isbool(false): 1 else: 0" "" "1";
    t "if_test10" "if isnum(false): 1 else: 0" "" "0";
    t "if_test11" "if isbool(1): 1 else: 0" "" "0";
    t "if_test12" "if (let x = 1, y = 2 in x < y): 3 else: 4" "" "3";
    (* if containing let in cond *)
    t "if_test13"
      "if (let x = 1, y = 2 in x < y): (let x = 2, y = 2 in x == y) else: (let x = 1, y = 2 in x > \
       y)"
      ""
      "true";
    t "if_test14" "if (false, 1)[0]: 1 else: 0" "" "0";
    (* if containing let in cond, then, else *)
    terr "if_error1" "if 1: 1 else: false" "" "if expected a boolean";
    terr "if_error2" "if add1(1): 1 else: false" "" "if expected a boolean";
    terr "if_error3" "if 1 + 1: 1 else: false" "" "if expected a boolean";
    terr "if_error4" "if (1,2): 1 else: false" "" "if expected a boolean";
      ]
;;

let let_tests =
  [ t "let_test1" "(let x = 10 in x)" "" "10";
    t "let_test2" "(let x=10,y=5 in x)" "" "10";
    t "let_test3" "(let x=10,y=5 in y)" "" "5";
    t "let_test4" "(let x=add1(10),y=5 in x)" "" "11";
    t "let_test5" "(let x=add1(69),y=add1(x) in y)" "" "71";
    t "let_test6" "(let x=add1(10),y=add1(x),z=add1(y) in z)" "" "13";
    t "let_test7" "(let x=add1(10),y=add1(x),z=add1(y) in x)" "" "11";
    t "let_test8" "(let x=add1((let x=add1(10),y=5 in x)),y=5 in x)" "" "12";
    t "let_test9" "(let x=(add1(let x=add1(10),y=5 in x)),y=(let z=add1(x) in z) in y)" "" "13";
    t "let_test10" "(let x=10,y=(let x=5 in x) in y)" "" "5";
    t "let_test11" "(let x=add1(10),y=add1(x),z=add1(y) in z)" "" "13";
    t "let_test12" "(let x=2*2,y=x - 1,z=y+2 in z * 2)" "" "10";
    t "let_test13" "(let x=(2+3)*2,y=10-(x+1) in y)" "" "-1";
    t "let_test14" "(let x=(2+3)*(1+1),y=(5+5)-(x+1) in y)" "" "-1";
    t "let_test15" "(let x=(2+3)*(1+1),y=(if true: x else: 0) in y)" "" "10";
    t "let_test16" "(let x = true, y = (if x: 1 else: 3) in y)" "" "1";
    t "let_test17" "(let x = -4 in (let x = x + 1 in x))" "" "-3";
    t "let_test18" "(let x = (let x = 1, y = 2 in x + y) in x)" "" "3";
    (* test that body expression uses env of its scope. *)
    t "let_test19" "(let x = 1, y = 2 in (let x = 3, y = 4 in x + y))" "" "7";
    (* test that inner bindings shadow outer bindings. *)
    t "let_test20" "(let x = 1 in (let x = 2 in x))" "" "2";
    t "let_test21" "(let x = 1 in (let y = 2 in x + y))" "" "3";
    t "let_test22" "(let x = (if (let x = 1, y = 2 in y > x): 3 else: 4), y = 5 in x + y)" "" "8";
    (* let bound if expression *)
    t "let_test23"
      "let x = (let x = 1 in x + 1), y = (let z = 2 in x + z), z = (if x > y: 5 else: 6) in x + y \
       + z"
      ""
      "12";
    t "let_test24" "let x=(1,2) in (let _ = x[0] := 3 in x)" "" "(3, 2)";
    (* complex let expression *) ]
;;

let let_and_print_tests =
  [ t "let_print_test1" "(let x=10, y=5 in print(add1(print(add1(x)))))" "" "11\n12\n12";
    t "let_print_test2" "(let x=10, y=5 in print(add1(print(add1(y)))))" "" "6\n7\n7";
    t "let_print_test3"
      "(let x=10, y=5, z=1, a=11, b=7, c=2, d=9 in print(x + y + z + a + b + c + d + \
       add1(print(add1(y)))))"
       ""
      "6\n52\n52";
    t "let_print_test4"
      "(let y=5, z=1, a=11, b=7, c=2, d=9 in print(y + z + a + b + c + d + add1(print(add1(y)))))"
      ""
      "6\n42\n42" ]
;;

let int_overflow_tests =
  [ (* terr "int_overflow1" (string_of_int (signed_int63_max + 2)) "Integer overflow"; *)
    terr "int_overflow1" "4611686018427387904" "" "The number literal";
    terr "int_overflow2" "-4611686018427387905" "" "The number literal" ]
;;

let tests =
  [ t "forty_one" "41" "" "41";
    t "forty" "40" "" "40";
    t "fals" "false" "" "false";
    t "tru" "true" "" "true" ]
;;

let program_tests =
  [ tvg "simple_program1" "def test(): 1 test()" "" "1";
    tvg "simple_program2" "def test(x): x test(1)" "" "1";
    tvg "simple_program3" "def test(a, b): a + b test(1, 2)" "" "3";
    tvg "simple_program4"
      "def test(a, b, c, d, e, f): a + b + c + d + e + f  test(1, 2, 3, 4, 5, 6)" "" "21";
    tvg "simple_program5"
      "def test(a, b, c, d, e, f, g): a + b + c + d + e + f + g  test(1, 2, 3, 4, 5, 6, 7)" "" "28";
    tvg "simple_program6"
      "def test(a, b, c, d, e, f, g, h): a + b + c + d + e + f + g + h test(1, 2, 3, 4, 5, 6, 7, 8)"
      ""
      "36";
    tvg "simple_program7" "def test(a, b, c, d, e, f, g, h): g test(1, 2, 3, 4, 5, 6, 7, 8)" "" "7";
    tvg "simple_program8" "def test(a, b, c, d, e, f, g, h): h test(1, 2, 3, 4, 5, 6, 7, 8)" "" "8";
    tvg "simple_program9" "def test(x): if x < 1 : 0 else: x + test(x - 1) test(1)" "" "1";
    tvg "simple_program10" "def test(x): if x < 1 : 0 else: x + test(x - 1) test(8)" "" "36";
    tvg "simple_program11"
      "def test(a, b, c, d, e, f, g, h): a + b + c + d + e + f + g + h \n\
      \  (let a=1,b=2,c=3,d=4,e=5,f=6,g=7,h=8 in test(a, b, c, d, e, f, g, h))" "" "36";
    tvg "simple_program12" "def test(x): x (let x=test(1) in x)" "" "1";
    tvg "simple_program13" "def test(x, y): x + y (let y=2 in (let x=test(1, 2) in x))" "" "3";
    tvg "simple_program14" "def test(x, y): x + y test(1,2) - test(3, 4)" "" "-4";
    tvg "simple_program15" "def test(x, y): x + y test(print(1),print(2))" "" "1\n2\n3";
    tvg "simple_program16" "def test(x): print(x) test(2)" "" "2\n2";
    tvg "simple_program17" "def test(x): let y = 1 in print(x) test(2)" "" "2\n2";
    tvg "simple_program18" "def test(x, y): print(x) let p = 1 in test(2, 3)" "" "2\n2";
    tvg "simple_program19" "def test(x, y): let p = 1 in print(x) let x = 1 in test(2, 3)" "" "2\n2";
    tvg "simple_program20" "def test((x, z), y): x test((1, 2), 3)" "" "1";
    tvg "simple_program21" "def test(x, y): x test((1, 2), 3)" "" "(1, 2)";
    tvg "simple_program22" "def test((x, z), y): y test((1, 2), 3)" "" "3";
    tvg "simple_program23" "def test((x, z), y): z test((1, 2), 3)" "" "2";
    ]
;;

let anf_tests =
  [ tanf "anf_test1" (parse_string "1+1" "1+1") ""
      (AProgram ([], ACExpr (CPrim2 (Plus, ImmNum (1L, ()), ImmNum (1L, ()), ())), ()));
    tanf "anf_test2"
      (parse_string "def" "def test(x): x + 1 test(1)") ""
      (AProgram
         ( [ADFun ("test", ["x"], ACExpr (CPrim2 (Plus, ImmId ("x", ()), ImmNum (1L, ()), ())), ())],
           ACExpr (CApp ("test", [ImmNum (1L, ())], Snake, ())),
           () ) );
    tanf "anf_test3"
      (parse_string "def" "def test(x): x + 1 test(1) + 1") ""
      (AProgram
         ( [ADFun ("test", ["x"], ACExpr (CPrim2 (Plus, ImmId ("x", ()), ImmNum (1L, ()), ())), ())],
           ALet
             ( "fun_3",
               CApp ("test", [ImmNum (1L, ())], Snake, ()),
               ACExpr (CPrim2 (Plus, ImmId ("fun_3", ()), ImmNum (1L, ()), ())),
               () ),
           () ) );
    tanf "anf_test4"
      (parse_string "def" "def test(a, b): a + b test(1, 2)") ""
      (AProgram
         ( [ ADFun
               ("test", ["a"; "b"], ACExpr (CPrim2 (Plus, ImmId ("a", ()), ImmId ("b", ()), ())), ())
           ],
           ACExpr (CApp ("test", [ImmNum (1L, ()); ImmNum (2L, ())], Snake, ())),
           () ) );
    tanf "anf_test5"
      (parse_string "def" "def test(a, b): a + b test(1 + 2, 4 + 3)") ""
      (AProgram
         ( [ ADFun
               ("test", ["a"; "b"], ACExpr (CPrim2 (Plus, ImmId ("a", ()), ImmId ("b", ()), ())), ())
           ],
           ALet
             ( "binop_2",
               CPrim2 (Plus, ImmNum (1L, ()), ImmNum (2L, ()), ()),
               ALet
                 ( "binop_5",
                   CPrim2 (Plus, ImmNum (4L, ()), ImmNum (3L, ()), ()),
                   ACExpr (CApp ("test", [ImmId ("binop_2", ()); ImmId ("binop_5", ())], Snake, ())),
                   () ),
               () ),
           () ) );
    tanf "anf_test6"
      (parse_string "def" "def test1(x): x + 1 def test2(x): x + 1 test1(1) + test2(3)") ""
      (AProgram
         ( [ ADFun ("test1", ["x"], ACExpr (CPrim2 (Plus, ImmId ("x", ()), ImmNum (1L, ()), ())), ());
             ADFun ("test2", ["x"], ACExpr (CPrim2 (Plus, ImmId ("x", ()), ImmNum (1L, ()), ())), ())
           ],
           ALet
             ( "fun_4",
               CApp ("test1", [ImmNum (1L, ())], Snake, ()),
               ALet
                 ( "fun_2",
                   CApp ("test2", [ImmNum (3L, ())], Snake, ()),
                   ACExpr (CPrim2 (Plus, ImmId ("fun_4", ()), ImmId ("fun_2", ()), ())),
                   () ),
               () ),
           () ) ) ]
;;

let stack_tests =
  [ tsa "naive_stack_test1" "let x=1 in x"
      ( AProgram
          ([], ALet ("x", CImmExpr (ImmNum (1L, 3)), ACExpr (CImmExpr (ImmId ("x", 2))), 1), 0),
        [("x", RegOffset (-8, RBP))] );
    tsa "naive_stack_test2" "let x=1,y=2 in x"
      ( AProgram
          ( [],
            ALet
              ( "x",
                CImmExpr (ImmNum (1L, 5)),
                ALet ("y", CImmExpr (ImmNum (2L, 4)), ACExpr (CImmExpr (ImmId ("x", 3))), 2),
                1 ),
            0 ),
        [("x", RegOffset (-8, RBP)); ("y", RegOffset (-16, RBP))] );
    tsa "naive_stack_test3" "let x=1 in let y=2 in x"
      ( AProgram
          ( [],
            ALet
              ( "x",
                CImmExpr (ImmNum (1L, 5)),
                ALet ("y", CImmExpr (ImmNum (2L, 4)), ACExpr (CImmExpr (ImmId ("x", 3))), 2),
                1 ),
            0 ),
        [("x", RegOffset (-8, RBP)); ("y", RegOffset (-16, RBP))] );
    tsa "naive_stack_test4" "1 + 1"
      (AProgram ([], ACExpr (CPrim2 (Plus, ImmNum (1L, 3), ImmNum (1L, 2), 1)), 0), []);
    tsa "naive_stack_test5" "def test1(y): y let x=1 in test1(x)"
      ( AProgram
          ( [ADFun ("test1", ["y"], ACExpr (CImmExpr (ImmId ("y", 6))), 5)],
            ALet ("x", CImmExpr (ImmNum (1L, 4)), ACExpr (CApp ("test1", [ImmId ("x", 3)], Snake, 2)), 1),
            0 ),
        [("y", RegOffset (16, RBP)); ("x", RegOffset (-8, RBP))] );
    tsa "naive_stack_test6" "def test1(y, z, a): y let x=1 in test1(x, 1, 2)"
      ( AProgram
          ( [ADFun ("test1", ["y"; "z"; "a"], ACExpr (CImmExpr (ImmId ("y", 8))), 7)],
            ALet
              ( "x",
                CImmExpr (ImmNum (1L, 6)),
                ACExpr (CApp ("test1", [ImmId ("x", 3); ImmNum (1L, 4); ImmNum (2L, 5)], Snake, 2)),
                1 ),
            0 ),
        [ ("y", RegOffset (16, RBP));
          ("z", RegOffset (24, RBP));
          ("a", RegOffset (32, RBP));
          ("x", RegOffset (-8, RBP)) ] );
    tsa "naive_stack_test7" "def test1(y, z, a): y let x=1 in test1(x, 1, 2) + 1"
      ( AProgram
          ( [ADFun ("test1", ["y"; "z"; "a"], ACExpr (CImmExpr (ImmId ("y", 12))), 11)],
            ALet
              ( "x",
                CImmExpr (ImmNum (1L, 10)),
                ALet
                  ( "fun_4",
                    CApp ("test1", [ImmId ("x", 7); ImmNum (1L, 8); ImmNum (2L, 9)], Snake, 6),
                    ACExpr (CPrim2 (Plus, ImmId ("fun_4", 5), ImmNum (1L, 4), 3)),
                    2 ),
                1 ),
            0 ),
        [ ("y", RegOffset (16, RBP));
          ("z", RegOffset (24, RBP));
          ("a", RegOffset (32, RBP));
          ("x", RegOffset (-8, RBP));
          ("fun_4", RegOffset (-16, RBP)) ] ) ]
;;

let is_well_formed_tests =
  [ terr "well_formed_arity1" "def test(a, b): a test(1)" "" "arity";
    terr "well_formed_arity2" "def test(a, b): a test(1, 2, 3)" "" "arity";
    terr "well_formed_arity3" "def test(): 1 test(3)" "" "arity";
    terr "well_formed_arity4" "def test(a): a test()" "" "arity";
    terr "well_formed_unbound_fun1" "def test(a, b): a test1(2, 1)" "" "function name test1, used";
    terr "well_formed_unbound_fun2" "def test(a, b): a def test2(b): test1(1, 2) 1" ""
      "function name test1, used";
    terr "well_formed_unbound_fun3" "def test(a, b): use() def use(): 1 test(2, 1)" ""
      "function name use, used";
    terr "well_formed_unbound_id1" "def test(a, b): a test(a, 1)" "" "identifier a, used";
    terr "well_formed_unbound_id2" "def test(a, b): c let c=1 in test(1, 2)" "" "identifier c, used";
    terr "well_formed_duplicate_id_in_let1" "let x=1,x=2 in x+x" "" "identifier x, redefined";
    tvg "well_formed_let1" "let x=1 in let x=x + 1 in x+x" "" "4";
    tvg "well_formed_let2" "let x=1 in let x=2 in x+x" "" "4";
    terr "well_formed_duplicate_id_in_arg1" "def test(a, a): a test(1, 1)" "" "identifier a, redefined";
    terr "well_formed_duplicate_id_in_arg2" "def test(b, a, a): a test(1, 1, 1)" ""
      "identifier a, redefined";
    terr "well_formed_duplicate_id_in_arg3" "def test(b, b, a): a test(1, 1, 1)" ""
      "identifier b, redefined";
    terr "well_formed_same_fun_name1" "def test(a, b): a def test(a, b): a test(3, 1)" ""
      "function name test, redefined";
    terr "well_formed_same_fun_name2" "def test(a, b): a def test(b): a test(2, 1)" ""
      "function name test, redefined";
    terr "well_formed_overflow1" "-4611686018427387905" "" "not supported";
    terr "well_formed_overflow2" "4611686018427387904" "" "not supported";
    terr "well_formed_mix1"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "not supported";
    terr "well_formed_mix2"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "arity";
    terr "well_formed_mix3"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "function name test1, redefined";
    terr "well_formed_mix4"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "function name test2, used";
    terr "well_formed_mix5"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "identifier a, redefined";
    terr "well_formed_mix5"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "identifier x, redefined";
    terr "well_formed_mix6"
      "def test(a, b): a def test1(a, a): a def test1(a, a): a test2(1) + test(1, 2, 3) + b + (let \
       x=1,x=2 in 4611686018427387904)"
       ""
      "identifier b, used";
    terr "well_formed_arity5" "def test((x, y), z): x test(1, 2, 2)" "" "arity";
    terr "well_formed_arity6" "def test((x, y), z): x test(1)" "" "arity";
    terr "well_formed_unbound_id3" "(x, 1, 2)" "" "identifier x, used";
    terr "well_formed_unbound_id4" "(0, 1, 2)[x]" "" "identifier x, used";
    terr "well_formed_unbound_id5" "(x, y, 2)" "" "identifier y, used";
    terr "well_formed_unbound_id6" "(x, y, 2)" "" "identifier x, used";
    ]
;;
let pair_tests = [
  t "tup1" "let t = (4, (5, 6)) in
            begin
              t[0] := 7;
              t
            end" "" "(7, (5, 6))";
  t "tup2" "let t = (4, (5, nil)) in
            begin
              t[1] := nil;
              t
            end" "" "(4, nil)";
  t "tup3" "let t = (4, (5, nil)) in
            begin
              t[1] := t;
              t
            end" "" "(4, <cyclic tuple 1>)";
  t "tup4" "let t = (4, 6) in
            (t, t)"
           ""
           "((4, 6), (4, 6))"

]

(* let oom = [
 *   tgcerr "oomgc1" (7) "(1, (3, 4))" "" "Out of memory";
 *   tgc "oomgc2" (8) "(1, (3, 4))" "" "(1, (3, 4))";
 *   tvgc "oomgc3" (8) "(1, (3, 4))" "" "(1, (3, 4))";
 *   tgc "oomgc4" (4) "(3, 4)" "" "(3, 4)";
 * ] *)

let input = [
    t "input1" "let x = input() in x + 2" "123" "125";
    t "input2" "print(input())" "5" "5\n5";
    t "input3" "print(input() + 1) + 1" "5" "6\n7";
    t "input4" "(input(), input())" "1\n2" "(1, 2)";
    t "input5" "input() + input()" "5\n5" "10";
    t "input6" "input()" "true" "true";
    t "input7" "input()" "false" "false";
    t "input8" "if input(): 0 else: 1" "false" "1";
    t "input9" "input()" "-1" "-1";
    terr "input_err1" "input()" "4611686018427387904" "overflowed";
    terr "input_err2" "input()" "hello" "Invalid input";
  ]

let tuple_tests = [
  t "tuple1" "let x=(1,) in 1" "" "1";
  t "tuple2" "(1,)" "" "(1,)";
  t "tuple3" "(1,2)" "" "(1, 2)";
  t "tuple4" "(1,2,3)" "" "(1, 2, 3)";
  t "tuple5" "(1,2,3)" "" "(1, 2, 3)";
  t "tuple6" "(true,)" "" "(true,)";
  t "tuple7" "(false,)" "" "(false,)";
  t "tuple8" "(-1,)" "" "(-1,)";
  t "tuple9" "let x=10 in (x,)" "" "(10,)";
  t "tuple10" "let x=10 in (x,)[0]" "" "10";
  t "tuple11" "(10,)[0]" "" "10";
  t "tuple12" "let _ = (10,)[0]:= 1 in 10" "" "10";
  t "tuple13" "let x = (1,2,3) in x[0] := 2 ; x[0]" "" "2";
  t "tuple14" "(1,2,3,4)[1]" "" "2";
  t "tuple15" "(1,2,3,4)[2]" "" "3";
  t "tuple16" "(1,2,3,4)[3]" "" "4";
  t "tuple17" "let x = (1,2,3) in x[1] := 5 ; x[1]" "" "5";
  t "tuple18" "let x = (1,2,3) in x[2] := 5 ; x[2]" "" "5";
  t "tuple19" "(1,(1,2))" "" "(1, (1, 2))";
  t "tuple20" "(1,(1,2))[1]" "" "(1, 2)";
  t "tuple21" "print((1,(1,2))[1])" "" "(1, 2)\n(1, 2)";
  t "tuple22" "print((1,(1,2)))[1]" "" "(1, (1, 2))\n(1, 2)";
  t "tuple23" "let (x, y) = (1, 2) in x" "" "1";
  t "tuple24" "let (x, y) = (1, 2) in y" "" "2";
  t "tuple25" "let (x, (_, (_, (_, y)))) = (1, (2, (3, (4, 5)))) in y" "" "5";
  t "tuple26" "let (x, (y, z)) = (1, (2, 3)) in y" "" "2";
  t "tuple27" "let (x, (y, z)) = (1, (2, 3)) in z" "" "3";
  t "tuple28" "let (x, (y, z)) = (1, (2, 3)) in x" "" "1";
  t "tuple29" "let (x, y) = (1, (2, 3)) in y" "" "(2, 3)";
  t "tuple30" "let (x, y) = (1, (2, 3)) in y[0] := 1" "" "nil";
  t "tuple31" "(print(1), print(2))" "" "1\n2\n(1, 2)";
  t "tuple32" "(print(1), print(2)) ; 3" "" "1\n2\n3";
  t "tuple33" "print(nil)" "" "nil\nnil";
  t "tuple34" "(print(1), print(2))[print(0)]" "" "1\n2\n0\n1";
  t "tuple35" "(print(1), print(2))[print(0)] := print(1+1)" "" "1\n2\n0\n2\nnil";
  t "tuple36" "def addpairs((x1, y1), (x2, y2)): (x1 + x2, y1 + y2) addpairs((1, 2), (3, 4))" "" "(4, 6)";
  terr "tuple_err1" "1[2]" "" "expected tuple";
  terr "tuple_err2" "(1,)[1]" "" "index too large";
  terr "tuple_err3" "(1,)[-1]" "" "index too small";
  terr "tuple_err4" "let x = (1,) in (x[0] := 2)[0]" "" "access component of nil";
  terr "tuple_err5" "let x = (1,) in (x[0] := 2)[0] := 1" "" "access component of nil";
  terr "tuple_err6" "def test((x, z), y): x test(1, 3)" "" "expected tuple";
  terr "tuple_err7" "(1, 2)[true]" "" "index is not a number";
  terr "tuple_err8" "(1, 2)[true] := 1" "" "index is not a number";
]

let equal_tests = [
  t "equal1" "equal(1, 1)" "" "true";
  t "equal2" "equal(2, 1)" "" "false";
  t "equal3" "equal(false, 1)" "" "false";
  t "equal4" "equal(true, false)" "" "false";
  t "equal5" "equal(true, true)" "" "true";
  t "equal6" "equal(false, false)" "" "true";
  t "equal7" "equal(-1, -1)" "" "true";
  t "equal8" "equal(-2, -1)" "" "false";
  t "equal9" "equal(2, (1, 2))" "" "false";
  t "equal10" "let x = (1,2) in equal(x, x)" "" "true";
  t "equal11" "equal((1, 2), (1, 2))" "" "true";
  t "equal12" "equal(false, true)" "" "false";
  t "equal13" "equal((1, (2, 3)), (1, (2, 3)))" "" "true";
  t "equal14" "equal((1, ((4, 5), 3)), (1, ((4, 5), 3)))" "" "true";
  t "equal15" "equal((1, ((4, 6), 3)), (1, ((4, 5), 3)))" "" "false";
  t "equal16" "let x = (1,2) in equal(x[0] := 2, 1)" "" "false";
  t "equal17" "let x = (1,2) in equal(1, x[0] := 2)" "" "false";
  t "equal18" "let x = (1,2) in equal(1, (1, x[0] := 2))" "" "false";
  t "equal19" "let x = (1,2) in equal((2, (3, (5, 6))), (1, 2))" "" "false";
  t "equal20" "let x = (1,2) in equal(x[1] := 1, x[0] := 2)" "" "true";
]

let sequences = [
  t "sequence1" "5+5 ; 2" "" "2";
  t "sequence2" "print(1) ; print(2) ; 3" "" "1\n2\n3";
]

let tuple_errors_thomas = [
  (*
    1) well-formed errors for tuples:
        - tuple shape mismatch when passing tuple as argument of function
        - tuple shape mismatch for let bind which is a tuple
    2) type error: throws error for each operation that doesn't take tuple as its arg, 
    3) accessing 'nil' signals error
  *)
  (* tuple shape mismatch when passing tuple as argument of function*)
   t "tuple_not_err1" "def f1((x1, x2)): x1 + x2 f1((1, 2, 3))" "" "3"; 
   t "tuple_not_err2" "let (b1, b2) = (1, 2, 3) in b1" "" "1";
   (* tuple shape mismatch for let bind which is a tuple*)
   terr "tuple_err3" "let (b1, b2, b3) = (1, 2) in 1" "" "index too large";
   (* type check errors: passing tuple to arguments for operations *)
   terr "tuple_err4" "add1((1, 2))" "" "expected a number";
   terr "tuple_err5" "sub1((1, 2))" "" "expected a number";
   terr "tuple_err6" "(1, 2) + 1" "" "expected a number";
   terr "tuple_err7" "1 + (1, 2)" "" "expected a number";
   terr "tuple_err8" "(1, 2) + (3, 4)" "" "expected a number";
   terr "tuple_err9" "(1, 2) - 1" "" "expected a number";
   terr "tuple_err10" "1 - (1, 2)" "" "expected a number";
   terr "tuple_err11" "(1, 2) - (3, 4)" "" "expected a number";
   terr "tuple_err12" "(1, 2) * 1" "" "expected a number";
   terr "tuple_err13" "1 * (1, 2)" "" "expected a number";
   terr "tuple_err14" "(1, 2) * (3, 4)" "" "expected a number";
   terr "tuple_err15" "(1, 2) && false" "" "expected";
   terr "tuple_err17" "(1, 2) > 2" "" "expected";
   terr "tuple_err18" "(1, 2) >= 2" "" "expected";
   terr "tuple_err19" "(1, 2) < 2" "" "expected";
   terr "tuple_err20" "(1, 2) <= 2" "" "expected";
   terr "tuple_err21" "2 > (1, 2)" "" "expected";
   terr "tuple_err22" "2 < (1, 2)" "" "expected";
   terr "tuple_err23" "2 >= (1, 2)" "" "expected";
   terr "tuple_err24" "2 <= (1, 2)" "" "expected";
   (* acessing 'nil' signals error *)
   terr "tuple_err25" "nil[0]" "" "nil";
   terr "tuple_err26" "let x = nil in x[0] := 3" "" "nil";
   
]

let suite =
"suite">:::
  prim1_tests @ prim2_tests @
  let_tests @ let_and_print_tests @ program_tests @
  tuple_tests @ input @ equal_tests @ let_tests @ sequences @ is_well_formed_tests @ 
  tuple_errors_thomas


let () =
  run_test_tt_main ("all_tests">:::[suite; input_file_test_suite ()])
;;

