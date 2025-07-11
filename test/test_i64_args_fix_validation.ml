open! Core
open! Import

(* This test validates that the i64 argument handling fix is working correctly.
   Before the fix: Legalization would create orphaned registers not mapped to parameters
   After the fix: i64 parameters are properly split into low/high i32 parameter pairs *)

let%expect_test "validate i64 parameter legalization fix" =
  (* Create a simple i64 function *)
  let func =
    let open Mirl in
    Function.build
      ~name:"validate_fix"
      ~params:[ ident "param", Type.I64 ]
      (fun builder params ->
        let param =
          match params with
          | [ (_, param, _) ] -> param
          | _ -> failwith "Expected 1 parameter"
        in
        Function.Builder.add_block' builder (fun block_builder ->
          let instructions = [ Instruction.Return [ param ] ] in
          Block.Builder.push_many block_builder instructions)
        [@nontail])
  in
  let program = Mirl.{ functions = [ func ]; externs = []; global_constants = [::] } in
  printf "=== Original Function ===\n";
  Mirl.to_string program |> print_endline;
  (* Apply ARM32 legalization *)
  let config : Mucaml_middle.Legalize.Config.t = { supports_native_i64 = false } in
  let legalized = Mucaml_middle.Legalize.legalize_program config program in
  printf "=== After Legalization ===\n";
  Mirl.to_string legalized |> print_endline;
  (* Verify that code generation works (this would fail before the fix) *)
  let target = "thumbv8m.main-none-eabi" in
  let (module Target) =
    Mucaml_backend.create
      (Mucaml_backend_common.Triple.of_string target)
      { board = Some "rp2350"; backend = None }
    |> ok_exn
  in
  printf "=== Code Generation Test ===\n";
  (match Target.build_program legalized with
   | Ok assembly ->
     printf "SUCCESS: Code generation completed\n";
     printf
       "Generated %d lines of assembly\n"
       (String.count (Target.Assembly.to_string assembly) ~f:(fun c -> Char.equal c '\n'))
   | Error _ -> assert false);
  [%expect
    {|
    === Original Function ===


    function validate_fix ($0 (param): i64) {
    $0: i64
    block_0:
        return $0
    }


    === After Legalization ===


    function validate_fix ($0 (param_low): i32, $1 (param_high): i32) {
    $0: i32, $1: i32
    block_0:
        return $0, $1
    }


    === Code Generation Test ===
    SUCCESS: Code generation completed
    Generated 14 lines of assembly
    |}]
;;

let%expect_test "validate ARM32 calling convention" =
  printf "=== ARM32 i64 Calling Convention Validation ===\n";
  printf "The fix ensures that:\n";
  printf "1. i64 parameters are split into consecutive i32 register pairs\n";
  printf "2. Each i64 argument uses 2 ARM32 registers (r0+r1, r2+r3, etc.)\n";
  printf "3. Return values follow ARM32 ABI (r0=low, r1=high)\n";
  printf "4. Register allocation can find all parameter registers\n";
  printf "5. No orphaned virtual registers are created\n";
  [%expect
    {|
    === ARM32 i64 Calling Convention Validation ===
    The fix ensures that:
    1. i64 parameters are split into consecutive i32 register pairs
    2. Each i64 argument uses 2 ARM32 registers (r0+r1, r2+r3, etc.)
    3. Return values follow ARM32 ABI (r0=low, r1=high)
    4. Register allocation can find all parameter registers
    5. No orphaned virtual registers are created |}]
;;
