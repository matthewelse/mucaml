open! Ox
open! Import
open Helpers

let%expect_test "example (monomorphic function)" =
  let ast = "let test a = 42" in
  let env = typecheck ast |> [%globalize: Env.t option] in
  print_s [%message (env : Env.t option)];
  [%expect
    {|
    (constraints ())
    ('_1) -> i32
    (env
     (((values ((test ((body (Var 0)) (quantifiers ()) (constraints ())))))
       (mut ((next_tv 2))))))
    |}]
;;

let%expect_test "example (polymorphic function)" =
  let ast = "let test a = a" in
  let env = typecheck ast |> [%globalize: Env.t option] in
  print_s [%message (env : Env.t option)];
  [%expect
    {|
    (constraints ())
    ('_1) -> '_1
    (env
     (((values ((test ((body (Var 0)) (quantifiers ()) (constraints ())))))
       (mut ((next_tv 2))))))
    |}]
;;

let%expect_test "example (unknown variable)" =
  let ast = "let test _ = a" in
  let env = typecheck ast |> [%globalize: Env.t option] in
  print_s [%message (env : Env.t option)];
  [%expect
    {|
    error: Unknown variable a
        ┌─ <test>:1:14
      1 │  let test _ = a
        │               ^ used here
    (env ())
    |}]
;;

let%expect_test "example (unknown construct)" =
  let ast = "let test _ = 1L" in
  let env = typecheck ast |> [%globalize: Env.t option] in
  print_s [%message (env : Env.t option)];
  [%expect
    {|
    error: Unsupported expression
        ┌─ <test>:1:14
      1 │  let test _ = 1L
        │               ^^ specified here
    (env ())
    |}]
;;
