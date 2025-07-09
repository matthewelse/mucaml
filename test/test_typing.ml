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
       ((test_false
         ((txt ((body (Var 8)) (quantifiers ()) (constraints ())))
          (loc ((start 89) (stop 99)))))
        (test_i32
         ((txt ((body (Var 0)) (quantifiers ()) (constraints ())))
          (loc ((start 4) (stop 12)))))
        (test_i64
         ((txt ((body (Var 2)) (quantifiers ()) (constraints ())))
          (loc ((start 24) (stop 32)))))
        (test_true
         ((txt ((body (Var 6)) (quantifiers ()) (constraints ())))
          (loc ((start 66) (stop 75)))))
        (test_unit
         ((txt ((body (Var 4)) (quantifiers ()) (constraints ())))
          (loc ((start 45) (stop 54)))))))
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
     ((values
       ((test
         ((txt ((body (Var 0)) (quantifiers ()) (constraints ())))
          (loc ((start 4) (stop 8)))))))
      (mut ((next_tv 2)))))
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
         ((txt
           ((body (Fun ((Base I32) (Base I32)) (Base I32))) (quantifiers ())
            (constraints ())))
          (loc ((start 15) (stop 34)))))
        (test
         ((txt ((body (Var 0)) (quantifiers ()) (constraints ())))
          (loc ((start 51) (stop 55)))))))
      (mut ((next_tv 5)))))
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
    (i32) -> i32
    (constraints
     ((Same_type (Fun ((Base I32) (Base I32)) (Base I32))
       (Fun ((Var 2) (Var 3)) (Var 4)) <opaque>)
      (Same_type (Fun ((Base I32) (Base I32)) (Base I32))
       (Fun ((Var 5) (Var 6)) (Var 7)) <opaque>)
      (Same_type (Var 1) (Var 5) <opaque>)
      (Same_type (Base I32) (Var 6) <opaque>)
      (Same_type (Var 7) (Var 2) <opaque>)
      (Same_type (Base I32) (Var 3) <opaque>)))
    (env
     ((values
       ((+
         ((txt
           ((body (Fun ((Base I32) (Base I32)) (Base I32))) (quantifiers ())
            (constraints ())))
          (loc ((start 15) (stop 34)))))
        (test
         ((txt ((body (Var 0)) (quantifiers ()) (constraints ())))
          (loc ((start 51) (stop 55)))))))
      (mut ((next_tv 8)))))
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
    (bool) -> i32
    (constraints
     ((Same_type (Fun ((Base I32) (Base I32)) (Base I32))
       (Fun ((Var 2) (Var 3)) (Var 4)) <opaque>)
      (Same_type (Base I32) (Var 2) <opaque>)
      (Same_type (Base I32) (Var 3) <opaque>)
      (Same_type (Var 4) (Base I32) <opaque>)
      (Same_type (Var 1) (Base Bool) <opaque>)
      (Same_type (Base I32) (Var 4) <opaque>)))
    (env
     ((values
       ((+
         ((txt
           ((body (Fun ((Base I32) (Base I32)) (Base I32))) (quantifiers ())
            (constraints ())))
          (loc ((start 15) (stop 34)))))
        (test
         ((txt ((body (Var 0)) (quantifiers ()) (constraints ())))
          (loc ((start 51) (stop 55)))))))
      (mut ((next_tv 5)))))
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
    (i32) -> i32
    (constraints
     ((Same_type (Fun ((Base I32)) (Base Bool)) (Fun ((Var 2)) (Var 3)) <opaque>)
      (Same_type (Base I32) (Var 2) <opaque>)
      (Same_type (Var 3) (Base Bool) <opaque>)
      (Same_type (Var 1) (Base I32) <opaque>)))
    (env
     ((values
       ((is_zero
         ((txt
           ((body (Fun ((Base I32)) (Base Bool))) (quantifiers ())
            (constraints ())))
          (loc ((start 17) (stop 30)))))
        (test
         ((txt ((body (Var 0)) (quantifiers ()) (constraints ())))
          (loc ((start 49) (stop 53)))))))
      (mut ((next_tv 4)))))
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
        │                 ^ ^ expected to have type i64, but has type i32.
        │                 │
        │                 has type (i64, i64) -> i64.
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
        │                 ^ ^^ expected to have type i32, but has type i64.
        │                 │
        │                 has type (i32, i32) -> i32.
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
        │                 ^ ^^^^ expected to have type i32, but has type bool.
        │                 │
        │                 has type (i32, i32) -> i32.
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
        │                 ^ expected to have type (i32, '_3) -> '_4, but has type (i32) -> i32.
    |}];
  test {|let test a = a a|};
  [%expect
    {|
    error: Type error
        ┌─ <test>:1:16
      1 │  let test a = a a
        │           -   ^ ^ expected to have type '_2, but has type ('_2) -> '_3.
        │           │   │
        │           │   has type ('_2) -> '_3.
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
        ┌─ <test>:5:5
      1 │  external ( + ) : (i64, i64) -> i64 = "add_i64"
        │                 ------------------- + was defined here
        ·
      4 │    let x = 3 in
        │        - x was defined here
      5 │    x + a
        │    ^ ^ has type (i64, i64) -> i64.
        │    │
        │    expected to have type i64, but has type i32.
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
        │                     ^ expected to have type i32, but has type bool.
    |}]
;;
