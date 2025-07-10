open! Ox
open! Import
open Helpers

let test text =
  let ast = text in
  let typed_ast = typecheck ast in
  match typed_ast with
  | None -> ()
  | Some typed_ast -> print_endline (Typed_ast.to_string_hum typed_ast)
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
    let test_i32 a : 'a : i32 =
      42

    let test_i64 a : 'a : i64 =
      42

    let test_unit a : 'a : unit =
      ()

    let test_true a : 'a : bool =
      true

    let test_false a : 'a : bool =
      false
    |}]
;;

let%expect_test "example: identity function" =
  test "let test a = a";
  [%expect {|
    let test a : 'a : 'a =
      $a
    |}]
;;

let%expect_test "example: application" =
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + 1|};
  [%expect
    {|
    external + : (i32, i32) -> i32 = "add_i32"

    let test a : i32 : i32 =
      app ($+, [$a, 1])
    |}]
;;

let%expect_test "example: let expression" =
  test
    {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a =
  let x = 3 in
  let y = 2 in
  a + x + y|};
  [%expect
    {|
    external + : (i32, i32) -> i32 = "add_i32"

    let test a : i32 : i32 =
      let x : i32 = 3 in
        let y : i32 = 2 in
          app ($+, [app ($+, [$a, $x]), $y])
    |}]
;;

let%expect_test "example: letrec expression" =
  test
    {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a =
  let rec x : i32 = 3 + x in
  if a then x else 9|};
  [%expect
    {|
    external + : (i32, i32) -> i32 = "add_i32"

    let test a : bool : i32 =
      let x : 'a = app ($+, [3, $x]) in
        if
          $a
        then
          $x
        else
          9
    |}]
;;

let%expect_test "example: if expression" =
  test
    {|external is_zero : i32 -> bool = "equal_i32"
let test a =
  let x = 3 in
  if is_zero x then x else a|};
  [%expect
    {|
    external is_zero : i32 -> bool = "equal_i32"

    let test a : i32 : i32 =
      let x : i32 = 3 in
        if
          app ($is_zero, [$x])
        then
          $x
        else
          $a
    |}]
;;

let%expect_test "error: application" =
  test {|external ( + ) : (i64, i64) -> i64 = "add_i64"
let test a = a + 1|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:2:18
      1 │  external ( + ) : (i64, i64) -> i64 = "add_i64"
        │                 ------------------- + was defined here
      2 │  let test a = a + 1
        │                 - ^ this is expected to have type i64, but has type i32.
        │                 │
        │                 this has type (i64, i64) -> i64.
        = Plain integers have type `i32`. `i64` literals look like `42L`.
    |}];
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + 1L|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:2:18
      1 │  external ( + ) : (i32, i32) -> i32 = "add_i32"
        │                 ------------------- + was defined here
      2 │  let test a = a + 1L
        │                 - ^^ this is expected to have type i32, but has type i64.
        │                 │
        │                 this has type (i32, i32) -> i32.
        = Integers with an L suffix have type `i64`. `i32` literals look like `42`.
    |}];
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a = a + true|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:2:18
      1 │  external ( + ) : (i32, i32) -> i32 = "add_i32"
        │                 ------------------- + was defined here
      2 │  let test a = a + true
        │                 - ^^^^ this is expected to have type i32, but has type bool.
        │                 │
        │                 this has type (i32, i32) -> i32.
    |}];
  test {|external ( + ) : (i32) -> i32 = "add_i32"
let test a = a + 1|};
  [%expect
    {|
    error: Incorrect number of arguments passed to a function.
        ┌─ <test>:2:16
      1 │  external ( + ) : (i32) -> i32 = "add_i32"
        │                 -------------- + was defined here
      2 │  let test a = a + 1
        │                 ^ this is expected to have type (i32, 'a) -> 'b, but has type i32 -> i32.
    |}];
  test {|let test a = a a|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:1:16
      1 │  let test a = a a
        │           -   - ^ this is expected to have type 'a, but has type 'a -> 'b.
        │           │   │
        │           │   this has type 'a -> 'b.
        │           a was defined here
        = This would create an infinitely nested type like `'a list list list list...`
        = we <3 recursion, but not _that_ much.
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

let%expect_test "error: let binding used incorrectly" =
  test
    {|external ( + ) : (i64, i64) -> i64 = "add_i64"

let test a = 
  let x = 3 in
  x + a|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:5:3
      1 │  external ( + ) : (i64, i64) -> i64 = "add_i64"
        │                 ------------------- + was defined here
        ·
      4 │    let x = 3 in
        │        - x was defined here
      5 │    x + a
        │    ^ - this has type (i64, i64) -> i64.
        │    │
        │    this is expected to have type i64, but has type i32.
        = Plain integers have type `i32`. `i64` literals look like `42L`.
    |}]
;;

let%expect_test "error: if expression" =
  (* TODO: it would be nice to show that `a` has type `bool` because it was used as a
     condition in an if expression. *)
  test {|let test a =
  let x = 3 in
  if a then x else a|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:3:20
      1 │  let test a =
        │           - a was defined here
      2 │    let x = 3 in

      3 │    if a then x else a
        │                     ^ this is expected to have type i32, but has type bool.
    |}]
;;
