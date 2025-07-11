open! Core
open! Import

(* Test that 64-bit return values work correctly on ARM32 *)

let test_arm32_codegen text =
  let target = "thumbv8m.main-none-eabi" in
  let (module Target) =
    Mucaml_backend.create
      (Mucaml_backend_common.Triple.of_string target)
      { board = Some "rp2350"; backend = None }
    |> ok_exn
  in
  let ast = Helpers.typecheck text |> Option.value_exn in
  let mirl = Mirl.of_ast ast in
  print_endline "=== Original MIRL ===";
  Mirl.to_string mirl |> print_endline;
  (* Apply legalization for ARM32 *)
  let config : Mucaml_middle.Legalize.Config.t = { supports_native_i64 = false } in
  let legalized = Mucaml_middle.Legalize.legalize_program config mirl in
  print_endline "=== Legalized MIRL ===";
  Mirl.to_string legalized |> print_endline;
  (* Generate ARM32 assembly *)
  let assembly = Target.build_program legalized |> Result.ok |> Option.value_exn in
  print_endline "=== ARM32 Assembly ===";
  Target.Assembly.to_string assembly |> print_endline
;;

let%expect_test "i64 return on ARM32" =
  test_arm32_codegen {| let main (_ : i64) = 1234567890L |};
  [%expect
    {|
    === Original MIRL ===


    function mucaml_main ($0 (_): i64) {
    $0: i64, $1: i64
    block_0:
        $1 := 1234567890
        return $1
    }


    === Legalized MIRL ===


    function mucaml_main ($0 (__low): i32, $1 (__high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32
    block_0:
        $2 := 1234567890
        $3 := 0
        return $2, $3
    }


    === ARM32 Assembly ===
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
      mov r0, #722
      movt r0, #18838
      mov r1, #0
      pop {pc}
    .fnend
    .size mucaml_main, . - mucaml_main
    |}]
;;
