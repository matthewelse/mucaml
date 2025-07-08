open! Ox
open! Import
open Helpers

let test text =
  let ast = text in
  let env = typecheck ast |> [%globalize: Env.t option] in
  match env with
  | None -> ()
  | Some env -> print_s [%message (env : Env.t)]
;;

let%expect_test "example (monomorphic functions)" =
  test
    {|let test_i32 a = 42
let test_i64 a = 42L
let test_unit a = ()
let test_true a = true
let test_false a = false|};
  [%expect
    {|
    ('_1) -> i32
    (constraints ())
    ('_3) -> i64
    (constraints ())
    ('_5) -> unit
    (constraints ())
    ('_7) -> bool
    (constraints ())
    ('_9) -> bool
    (constraints ())
    (env
     ((values
       ((test_false ((body (Var 8)) (quantifiers ()) (constraints ())))
        (test_i32 ((body (Var 0)) (quantifiers ()) (constraints ())))
        (test_i64 ((body (Var 2)) (quantifiers ()) (constraints ())))
        (test_true ((body (Var 6)) (quantifiers ()) (constraints ())))
        (test_unit ((body (Var 4)) (quantifiers ()) (constraints ())))))
      (mut ((next_tv 10)))))
    |}]
;;

let%expect_test "example (polymorphic function)" =
  test "let test a = a";
  [%expect
    {|
    ('_1) -> '_1
    (constraints ())
    (env
     ((values ((test ((body (Var 0)) (quantifiers ()) (constraints ())))))
      (mut ((next_tv 2)))))
    |}]
;;

let%expect_test "error: unknown variable" =
  test "let test _ = a";
  [%expect
    {|
    error: Unknown variable a
        ┌─ <test>:1:14
      1 │  let test _ = a
        │               ^ used here
    |}]
;;

let%expect_test "error: unsupported expression" =
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + 1|};
  [%expect
    {|
    ('_1) -> '_2
    (constraints
     ((Same_type (Fun ((Base I32) (Base I32)) (Base I32))
       (Fun ((Var 1) (Base I32)) (Var 2)))))
    (env
     ((values
       ((+
         ((body (Fun ((Base I32) (Base I32)) (Base I32))) (quantifiers ())
          (constraints ())))
        (test ((body (Var 0)) (quantifiers ()) (constraints ())))))
      (mut ((next_tv 3)))))
    |}]
;;

let%expect_test "error: unknown construct" =
  test "external test : i32 -> i32 = \"test\"";
  [%expect
    {|
    (env
     ((values
       ((test
         ((body (Fun ((Base I32)) (Base I32))) (quantifiers ()) (constraints ())))))
      (mut ((next_tv 0)))))
    |}]
;;
