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
      push {lr}
    mucaml_main__block_0:
      mov r0, #10
      mov r1, #32
      add r0, r0, r1
      pop {pc}
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
      push {lr}
    mucaml_main__block_0:
      mov r0, #34464
      movt r0, #1
      pop {pc}
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
      push {lr}
    mucaml_main__block_0:
      mov r0, #0
      mov r1, #32
      sub r0, r0, r1
      pop {pc}
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
      push {lr}
    mucaml_main__block_0:
      mov r0, #100
      bl sleep_ms
      mov r0, #7
      bl led_on
      mov r0, #100
      bl sleep_ms
      mov r0, #7
      bl led_off
      mov r0, #0
      pop {pc}
    .fnend
    .size mucaml_main, . - mucaml_main
    |}];
  test {|
    let main x : int32 =
      if x then (3 + 4) else (5 + 6)
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
      push {lr}
    mucaml_main__block_0:
      cbnz r0, mucaml_main__block_2
      b mucaml_main__block_3
    mucaml_main__block_1:
      pop {pc}
    mucaml_main__block_2:
      mov r0, #3
      mov r1, #4
      add r0, r0, r1
      b mucaml_main__block_1
    mucaml_main__block_3:
      mov r0, #5
      mov r1, #6
      add r0, r0, r1
      b mucaml_main__block_1
    .fnend
    .size mucaml_main, . - mucaml_main
    |}];
  (* Save caller saved registers *)
  test
    {|
    external f : int32 -> int32 = "f1"

    let main x : int32 =
      let y = f 100 in
      let z = f x in
      let a = f y in
      a + z + y + x
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
      push {lr,r4,r5,r6}
      mov r4, r0
    mucaml_main__block_0:
      mov r0, #100
      bl f1
      mov r5, r0
      mov r0, r4
      bl f1
      mov r6, r0
      mov r0, r5
      bl f1
      add r0, r0, r6
      add r0, r0, r5
      add r0, r0, r4
      pop {pc,r4,r5,r6}
    .fnend
    .size mucaml_main, . - mucaml_main
    |}];
  (* Prefer callee saved registers for variables that span function calls. *)
  test
    {|
    external f : int32 -> int32 = "f1"

    let main x : int32 =
      let z = f x in
      let y = f 100 in
      let a = f y in
      a + z + y
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
      push {lr,r4,r5}
    mucaml_main__block_0:
      bl f1
      mov r4, r0
      mov r0, #100
      bl f1
      mov r5, r0
      mov r0, r5
      bl f1
      add r0, r0, r4
      add r0, r0, r5
      pop {pc,r4,r5}
    .fnend
    .size mucaml_main, . - mucaml_main
    |}]
;;
