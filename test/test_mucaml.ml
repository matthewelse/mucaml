open! Core

let test text = Helpers.compile text

let%expect_test _ =
  (* Use [mov] for small immediates. *)
  test {| let main _ : int32 = 10 + 32 |};
  [%expect
    {|
    .syntax unified
    .cpu cortex-m33
    .thumb

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
    .syntax unified
    .cpu cortex-m33
    .thumb

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
    .syntax unified
    .cpu cortex-m33
    .thumb

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
    |}];
  test
    {|
    external sleep_ms : int32 -> unit = "sleep_ms"
    external led_on : int32 -> unit = "led_on"
    external led_off : int32 -> unit = "led_off"

    let main x : int32 =
      let _ = sleep_ms 100 in
      let _ = led_on 7 in
      let _ = sleep_ms 100 in
      let _ = led_off 7 in
      0     
    |};
  [%expect
    {|
    .syntax unified
    .cpu cortex-m33
    .thumb

    .thumb_func
    .type mucaml_main, %function
    .globl mucaml_main
    .fnstart
    mucaml_main:
      push {r4}
      mov r0, #100
      push {r0}
      bl sleep_ms
      pop {r0}
      mov r1, #7
      push {r0}
      mov r0, r1
      bl led_on
      pop {r0}
      mov r2, #100
      push {r0}
      mov r0, r2
      bl sleep_ms
      pop {r0}
      mov r3, #7
      push {r0}
      mov r0, r3
      bl led_off
      pop {r0}
      mov r4, #0
      mov r0, r4
      bx lr
      pop {r4}
    .fnend
    .size mucaml_main, . - mucaml_main
    |}]
;;
