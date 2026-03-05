open! Core
open! Currency_conversion_mnemonics

let print_result rate =
  let result = Algorithm.calculate ~correct_rate:rate () in
  match result with
  | Error msg -> print_endline ("Error: " ^ msg)
  | Ok mnemonic ->
      print_endline (Sexp.to_string_hum (Mnemonic.sexp_of_t mnemonic))

let%expect_test "rate 2" =
  print_result (Q.of_int 2);
  [%expect {| ((correct_rate 2) (steps ((Times 2)))) |}]

let%expect_test "rate 10" =
  print_result (Q.of_int 10);
  [%expect {| ((correct_rate 10) (steps ((Times 10)))) |}]

let%expect_test "rate 0.5" =
  print_result (Q.of_ints 1 2);
  [%expect {| ((correct_rate 1/2) (steps ((Divide 2)))) |}]

let%expect_test "rate 0.1" =
  print_result (Q.of_ints 1 10);
  [%expect {| ((correct_rate 1/10) (steps ((Divide 10)))) |}]

let%expect_test "rate 0.2" =
  print_result (Q.of_ints 1 5);
  [%expect {| ((correct_rate 1/5) (steps ((Times 2) (Divide 10)))) |}]

let%expect_test "rate 5" =
  print_result (Q.of_int 5);
  [%expect {| ((correct_rate 5) (steps ((Divide 2) (Times 10)))) |}]

let%expect_test "rate 20" =
  print_result (Q.of_int 20);
  [%expect {| ((correct_rate 20) (steps ((Times 2) (Times 10)))) |}]

let%expect_test "rate 6" =
  print_result (Q.of_int 6);
  [%expect {| ((correct_rate 6) (steps ((Times 3) (Times 2)))) |}]

let%expect_test "rate 1.5" =
  print_result (Q.of_ints 3 2);
  [%expect {| ((correct_rate 3/2) (steps ((Times 3) (Divide 2)))) |}]

let%expect_test "rate 0.3" =
  print_result (Q.of_ints 3 10);
  [%expect {| ((correct_rate 3/10) (steps ((Times 3) (Divide 10)))) |}]

let%expect_test "rate 30" =
  print_result (Q.of_int 30);
  [%expect {| ((correct_rate 30) (steps ((Times 3) (Times 10)))) |}]

let%expect_test "rate 15" =
  print_result (Q.of_int 15);
  [%expect
    {| ((correct_rate 15) (steps ((Times 2) (Times 2) (Times 2) (Times 2)))) |}]

let%expect_test "rate 0.6" =
  print_result (Q.of_ints 3 5);
  [%expect {| ((correct_rate 3/5) (steps ((Times 3) (Times 2) (Divide 10)))) |}]

let%expect_test "rate 4" =
  print_result (Q.of_int 4);
  [%expect {| ((correct_rate 4) (steps ((Times 2) (Times 2)))) |}]

let%expect_test "rate 0.25" =
  print_result (Q.of_ints 1 4);
  [%expect {| ((correct_rate 1/4) (steps ((Divide 2) (Divide 2)))) |}]

(* Tests where perfect solution cannot be found *)

let%expect_test "rate 7 (imperfect)" =
  print_result (Q.of_int 7);
  [%expect {| ((correct_rate 7) (steps ((Times 2) (Times 2) (Times 2)))) |}]

let%expect_test "rate 11 (imperfect)" =
  print_result (Q.of_int 11);
  [%expect {| ((correct_rate 11) (steps ((Times 10)))) |}]

let%expect_test "rate 0.14285714285714285 (1/7, imperfect)" =
  print_result (Q.of_ints 1 7);
  [%expect
    {| ((correct_rate 1/7) (steps ((Times 3) (Divide 2) (Divide 10)))) |}]

let%expect_test "rate 1.4 (7/5, imperfect)" =
  print_result (Q.of_ints 7 5);
  [%expect {| ((correct_rate 7/5) (steps ())) |}]

let%expect_test "rate 13 (imperfect)" =
  print_result (Q.of_int 13);
  [%expect {| ((correct_rate 13) (steps ((Times 10)))) |}]

let%expect_test "rate 0.7 (7/10, imperfect)" =
  print_result (Q.of_ints 7 10);
  [%expect
    {| ((correct_rate 7/10) (steps ((Times 2) (Times 2) (Times 2) (Divide 10)))) |}]
