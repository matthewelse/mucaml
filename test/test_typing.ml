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

let%expect_test "example: const functions" =
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

let%expect_test "example: identity function" =
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

let%expect_test "error: application" =
  test {|external ( + ) : (i64, i64) -> i64 = "add_i64"
let test a = a + 1|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:2:18
      2 │  let test a = a + 1
        │                 ^ ^ expected to have type i64, but has type i32.
        │                 │
        │                 has type (i64,i64) -> i64.
        = Plain integers are i32s. i64 literals look like 1L.
    |}];
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + 1L|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:2:18
      2 │  let test a = a + 1L
        │                 ^ ^^ expected to have type i32, but has type i64.
        │                 │
        │                 has type (i32,i32) -> i32.
        = Integers with an L suffix are i64s. i32 literals look like 1.
    |}];
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + true|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:2:18
      2 │  let test a = a + true
        │                 ^ ^^^^ expected to have type i32, but has type bool.
        │                 │
        │                 has type (i32,i32) -> i32.
    |}];
  test {|external ( + ) : (i32) -> i32 = "add_i32"
let test a = a + 1|};
  [%expect
    {|
    error: Incorrect number of arguments passed to a function.
        ┌─ <test>:2:16
      2 │  let test a = a + 1
        │                 ^ expected to have type (i32,'_3) -> '_4, but has type (i32) -> i32.
    |}];
  test {|let test a = a a|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:1:16
      1 │  let test a = a a
        │               ^ ^ expected to have type '_2, but has type ('_2) -> '_3.
        │               │
        │               has type ('_2) -> '_3.
        = This would create an infinitely nested type like `'a list list list list...`
        = we <3 recursion, but not _that_ much.
    |}]
;;

let%expect_test "example: application" =
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + 1|};
  [%expect
    {|
    (i32) -> i32
    (constraints
     ((Same_type (Fun ((Base I32) (Base I32)) (Base I32))
       (Fun ((Var 2) (Var 3)) (Var 4)) <opaque>)
      (Same_type (Var 1) (Var 2) <opaque>)
      (Same_type (Base I32) (Var 3) <opaque>)))
    (env
     ((values
       ((+
         ((body (Fun ((Base I32) (Base I32)) (Base I32))) (quantifiers ())
          (constraints ())))
        (test ((body (Var 0)) (quantifiers ()) (constraints ())))))
      (mut ((next_tv 5)))))
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
