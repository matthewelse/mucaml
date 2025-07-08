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
  test "let test a = 42";
  [%expect
    {|
    (constraints ())
    ('_1) -> i32
    (env
     ((values ((test ((body (Var 0)) (quantifiers ()) (constraints ())))))
      (mut ((next_tv 2)))))
    |}]
;;

let%expect_test "example (polymorphic function)" =
  test "let test a = a";
  [%expect
    {|
    (constraints ())
    ('_1) -> '_1
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

let%expect_test "error: unknown construct" =
  test "external test : i32 -> i32 = \"test\"";
  [%expect
    {|
    error: Unsupported external
        ┌─ <test>:1:1
      1 │  external test : i32 -> i32 = "test"
        │  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ specified here
        = The FFI isn't supported by the type checker yet.
    |}]
;;
