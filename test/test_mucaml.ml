open! Core

let test text = Helpers.compile text

let%expect_test _ =
  (* Use [mov] for small immediates. *)
  test {| let main _ : int32 = 10 + 32 |};
  [%expect
    {|
    .thumb_func
    .type mucaml_main, %function
    .globl mucaml_main
    .fnstart
    mucaml_main:
      mov r0, #10
      mov r1, #32
      add r2, r0, r1
      mov r0, r2
      bx lr
    .fnend
    .size mucaml_main, . - mucaml_main
    |}];
  (* Use [mov, movt] to represent large immediates. *)
  test {| let main _ : int32 = 100000 |};
  [%expect
    {|
    .thumb_func
    .type mucaml_main, %function
    .globl mucaml_main
    .fnstart
    mucaml_main:
      mov r0, #34464
      movt r0, #1
      bx lr
    .fnend
    .size mucaml_main, . - mucaml_main
    |}];
  (* Negative constants are just [0 - (abs x)] *)
  test {| let main _ : int32 = (~32) |};
  [%expect
    {|
    .thumb_func
    .type mucaml_main, %function
    .globl mucaml_main
    .fnstart
    mucaml_main:
      mov r0, #0
      mov r1, #32
      sub r2, r0, r1
      mov r0, r2
      bx lr
    .fnend
    .size mucaml_main, . - mucaml_main
    |}]
;;
