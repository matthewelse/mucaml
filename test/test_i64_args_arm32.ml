open! Core
open! Import

(* Test 64-bit argument handling on ARM32 - this should reveal issues with parameter passing *)

let test_i64_args_codegen text =
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

(* Create MIRL functions manually since frontend doesn't support multiple parameters yet *)

let create_i64_identity_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_identity"
    ~params:[ ident "x", Type.I64 ]
    (fun builder params ->
      let x =
        match params with
        | [ (_, x, _) ] -> x
        | _ -> failwith "Expected 1 parameter"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let instructions = [ Instruction.Return [ x ] ] in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

let create_i64_add_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_add"
    ~params:[ ident "a", Type.I64; ident "b", Type.I64 ]
    (fun builder params ->
      let a, b =
        match params with
        | [ (_, a, _); (_, b, _) ] -> a, b
        | _ -> failwith "Expected 2 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Add { dst = result; src1 = a; src2 = b }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

let create_mixed_i32_i64_add_function () =
  let open Mirl in
  Function.build
    ~name:"test_mixed_add"
    ~params:[ ident "a", Type.I32; ident "b", Type.I64 ]
    (fun builder params ->
      let a, b =
        match params with
        | [ (_, a, _); (_, b, _) ] -> a, b
        | _ -> failwith "Expected 2 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Add { dst = result; src1 = a; src2 = b }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

let test_i64_args_mirl_codegen name create_func =
  printf "=== Testing %s ===\n" name;
  let func = create_func () in
  let program = Mirl.{ functions = [ func ]; externs = []; global_constants = [::] } in
  printf "=== Original MIRL ===\n";
  Mirl.to_string program |> print_endline;
  (* Apply ARM32 legalization *)
  let config : Mucaml_middle.Legalize.Config.t = { supports_native_i64 = false } in
  let legalized = Mucaml_middle.Legalize.legalize_program config program in
  printf "=== Legalized MIRL ===\n";
  Mirl.to_string legalized |> print_endline;
  (* Test code generation *)
  let target = "thumbv8m.main-none-eabi" in
  let (module Target) =
    Mucaml_backend.create
      (Mucaml_backend_common.Triple.of_string target)
      { board = Some "rp2350"; backend = None }
    |> ok_exn
  in
  let assembly = Target.build_program legalized |> Result.ok |> Option.value_exn in
  printf "=== ARM32 Assembly ===\n";
  Target.Assembly.to_string assembly |> print_endline
;;

(* Test simple i64 parameter function *)
let%expect_test "i64 identity parameter on ARM32" =
  test_i64_args_mirl_codegen "I64 Identity" create_i64_identity_function;
  [%expect
    {|
    === Testing I64 Identity ===
    === Original MIRL ===


    function test_i64_identity ($0 (x): i64) {
    $0: i64
    block_0:
        return $0
    }


    === Legalized MIRL ===


    function test_i64_identity ($0 (x_low): i32, $1 (x_high): i32) {
    $0: i32, $1: i32
    block_0:
        return $0, $1
    }


    === ARM32 Assembly ===
    .syntax unified
    .cpu cortex-m33
    .thumb

    .thumb_func
    .type test_i64_identity, %function
    .globl test_i64_identity
    .fnstart
    test_i64_identity:
      push {lr}
    test_i64_identity__block_0:
      pop {pc}
    .fnend
    .size test_i64_identity, . - test_i64_identity
    |}]
;;

(* Test i64 addition with parameters *)
let%expect_test "i64 add parameters on ARM32" =
  test_i64_args_mirl_codegen "I64 Add" create_i64_add_function;
  [%expect
    {|
    === Testing I64 Add ===
    === Original MIRL ===


    function test_i64_add ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $2 := $0 + $1
        return $2
    }


    === Legalized MIRL ===


    function test_i64_add ($0 (a_low): i32, $1 (a_high): i32, $2 (b_low): i32, $3 (b_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32
    block_0:
        $4 := $0 + $2
        $5 := $1 +c $3
        return $4, $5
    }


    === ARM32 Assembly ===
    .syntax unified
    .cpu cortex-m33
    .thumb

    .thumb_func
    .type test_i64_add, %function
    .globl test_i64_add
    .fnstart
    test_i64_add:
      push {lr}
      mov r12, r1
      mov r1, r2
      mov r2, r12
    test_i64_add__block_0:
      add r1, r0, r1
      adc r3, r2, r3
      mov r0, r1
      mov r1, r3
      pop {pc}
    .fnend
    .size test_i64_add, . - test_i64_add
    |}]
;;

(* Test mixed i32/i64 parameters *)
let%expect_test "mixed i32 i64 parameters on ARM32" =
  test_i64_args_mirl_codegen "Mixed I32/I64" create_mixed_i32_i64_add_function;
  [%expect
    {|
    === Testing Mixed I32/I64 ===
    === Original MIRL ===


    function test_mixed_add ($0 (a): i32, $1 (b): i64) {
    $0: i32, $1: i64, $2: i64
    block_0:
        $2 := $0 + $1
        return $2
    }


    === Legalized MIRL ===


    function test_mixed_add ($0 (a): i32, $1 (b_low): i32, $2 (b_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32
    block_0:
        $3 := $0 + $1
        $4 := $0 +c $2
        return $3, $4
    }


    === ARM32 Assembly ===
    .syntax unified
    .cpu cortex-m33
    .thumb

    .thumb_func
    .type test_mixed_add, %function
    .globl test_mixed_add
    .fnstart
    test_mixed_add:
      push {lr}
      mov r12, r0
      mov r0, r1
      mov r1, r12
    test_mixed_add__block_0:
      add r0, r1, r0
      adc r2, r1, r2
      mov r1, r2
      pop {pc}
    .fnend
    .size test_mixed_add, . - test_mixed_add
    |}]
;;
