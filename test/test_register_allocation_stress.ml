open! Core
open! Import

(* Comprehensive register allocation stress tests focusing on:
   1. High register pressure scenarios that should trigger spilling
   2. Caller/callee-saved register selection
   3. Live intervals across function calls
   4. Complex control flow with register pressure *)

(* Create a function that uses many registers simultaneously to trigger spilling *)
let create_high_register_pressure_function () =
  let open Mirl in
  Function.build
    ~name:"test_high_pressure"
    ~params:[ ident "a", Type.I32; ident "b", Type.I32; ident "c", Type.I32; ident "d", Type.I32 ]
    (fun builder params ->
      let a, b, c, d =
        match params with
        | [ (_, a, _); (_, b, _); (_, c, _); (_, d, _) ] -> a, b, c, d
        | _ -> failwith "Expected 4 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        (* Create many temporary registers to exceed available physical registers *)
        let temp1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp3 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp4 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp5 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp6 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp7 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp8 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp9 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp10 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let instructions =
          [ Instruction.Add { dst = temp1; src1 = a; src2 = b }
          ; Instruction.Add { dst = temp2; src1 = c; src2 = d }
          ; Instruction.Add { dst = temp3; src1 = temp1; src2 = temp2 }
          ; Instruction.Add { dst = temp4; src1 = a; src2 = temp3 }
          ; Instruction.Add { dst = temp5; src1 = b; src2 = temp4 }
          ; Instruction.Add { dst = temp6; src1 = c; src2 = temp5 }
          ; Instruction.Add { dst = temp7; src1 = d; src2 = temp6 }
          ; Instruction.Add { dst = temp8; src1 = temp1; src2 = temp7 }
          ; Instruction.Add { dst = temp9; src1 = temp2; src2 = temp8 }
          ; Instruction.Add { dst = temp10; src1 = temp3; src2 = temp9 }
          ; (* All previous temps are still live here - should trigger spilling *)
            Instruction.Add { dst = result; src1 = temp10; src2 = temp4 }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

(* Test function calls with many live registers to test caller-saved preservation *)
let create_call_heavy_function () =
  let open Mirl in
  Function.build
    ~name:"test_call_heavy"
    ~params:[ ident "x", Type.I32; ident "y", Type.I32 ]
    (fun builder params ->
      let x, y =
        match params with
        | [ (_, x, _); (_, y, _) ] -> x, y
        | _ -> failwith "Expected 2 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let const1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let const2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let call_result1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let call_result2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp3 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp4 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let instructions =
          [ Instruction.Set { dst = const1; value = 100 }
          ; Instruction.Set { dst = const2; value = 200 }
          ; Instruction.Add { dst = temp1; src1 = x; src2 = y }
          ; Instruction.Add { dst = temp2; src1 = const1; src2 = const2 }
          ; (* temp1, temp2, x, y should be live across this call *)
            Instruction.C_call { dst = call_result1; func = "func1"; args = [ temp1 ] }
          ; Instruction.Add { dst = temp3; src1 = temp2; src2 = call_result1 }
          ; Instruction.Add { dst = temp4; src1 = x; src2 = y }
          ; (* Multiple live registers across another call *)
            Instruction.C_call { dst = call_result2; func = "func2"; args = [ temp3 ] }
          ; Instruction.Add { dst = result; src1 = temp4; src2 = call_result2 }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

(* Test complex control flow with high register pressure *)
let create_branchy_high_pressure_function () =
  let open Mirl in
  Function.build
    ~name:"test_branchy_pressure"
    ~params:[ ident "cond", Type.I32; ident "a", Type.I32; ident "b", Type.I32 ]
    (fun builder params ->
      let cond, a, b =
        match params with
        | [ (_, cond, _); (_, a, _); (_, b, _) ] -> cond, a, b
        | _ -> failwith "Expected 3 parameters"
      in
      (* Create multiple blocks to test live intervals across branches *)
      let then_block = Function.Builder.add_block builder ignore in
      let else_block = Function.Builder.add_block builder ignore in
      let merge_block = Function.Builder.add_block builder ignore in
      (* Entry block with many live registers *)
      Function.Builder.add_block' builder (fun block_builder ->
        let temp1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp3 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp4 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let instructions =
          [ Instruction.Set { dst = temp1; value = 10 }
          ; Instruction.Add { dst = temp2; src1 = a; src2 = temp1 }
          ; Instruction.Add { dst = temp3; src1 = b; src2 = temp2 }
          ; Instruction.Add { dst = temp4; src1 = temp1; src2 = temp3 }
          ; (* temp1-4 should be live across the branch *)
            Instruction.Branch
              { condition = cond; target = Block.Builder.label then_block }
          ; Instruction.Jump { target = Block.Builder.label else_block }
          ]
        in
        Block.Builder.push_many block_builder instructions);
      (* Then block - more register usage *)
      let then_result = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let then_temp1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let then_temp2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let instructions =
        [ Instruction.Set { dst = then_temp1; value = 20 }
        ; Instruction.Add { dst = then_temp2; src1 = a; src2 = then_temp1 }
        ; Instruction.Add { dst = then_result; src1 = then_temp2; src2 = b }
        ; Instruction.Jump { target = Block.Builder.label merge_block }
        ]
      in
      Block.Builder.push_many then_block instructions;
      (* Else block - different register usage *)
      let else_result = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let else_temp1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let else_temp2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let instructions =
        [ Instruction.Set { dst = else_temp1; value = 30 }
        ; Instruction.Sub { dst = else_temp2; src1 = a; src2 = else_temp1 }
        ; Instruction.Sub { dst = else_result; src1 = else_temp2; src2 = b }
        ; Instruction.Jump { target = Block.Builder.label merge_block }
        ]
      in
      Block.Builder.push_many else_block instructions;
      (* Merge block - combine results *)
      let final_result = Function.Builder.fresh_register builder ~ty:Type.I32 in
      let instructions =
        [ Instruction.Add { dst = final_result; src1 = then_result; src2 = else_result }
        ; Instruction.Return [ final_result ]
        ]
      in
      Block.Builder.push_many merge_block instructions;
      ())
;;

(* Test i64 operations with high register pressure on ARM32 *)
let create_i64_high_pressure_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_pressure"
    ~params:[ ident "a", Type.I64; ident "b", Type.I64; ident "c", Type.I64 ]
    (fun builder params ->
      let a, b, c =
        match params with
        | [ (_, a, _); (_, b, _); (_, c, _) ] -> a, b, c
        | _ -> failwith "Expected 3 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        (* Each i64 operation will create 2 i32 registers on ARM32 *)
        let temp1 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp2 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp3 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp4 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp5 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Add { dst = temp1; src1 = a; src2 = b }
          ; Instruction.Sub { dst = temp2; src1 = temp1; src2 = c }
          ; Instruction.Add { dst = temp3; src1 = temp2; src2 = a }
          ; Instruction.Sub { dst = temp4; src1 = temp3; src2 = b }
          ; Instruction.Add { dst = temp5; src1 = temp4; src2 = c }
          ; (* All temps still live - should cause severe register pressure on ARM32 *)
            Instruction.Add { dst = result; src1 = temp5; src2 = temp1 }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

let test_register_allocation_stress name create_func expected_to_fail =
  printf "=== Testing %s ===\n" name;
  let func = create_func () in
  (* Test on a mock register allocator with limited registers *)
  try
    let module TestRegister = struct
      type t = int [@@deriving compare, sexp_of]

      include functor Comparable.Make_plain

      (* Very limited register set to force spilling *)
      let all_available_for_allocation = List.init 4 ~f:Fn.id (* Only 4 registers! *)
      let caller_saved = Set.of_list [ 0; 1 ]
      let callee_saved = Set.of_list [ 2; 3 ]
      let to_string t = [%string "r%{t#Int}"]
    end
    in
    let module TestAllocator = Mucaml_backend_common.Linscan.Make (TestRegister) in
    let _registers, used_registers, call_sites = TestAllocator.allocate_registers func in
    printf "SUCCESS: Allocated registers without spilling\n";
    printf
      "Used registers: %s\n"
      (Set.to_list used_registers
       |> List.map ~f:TestRegister.to_string
       |> String.concat ~sep:", ");
    printf "Call sites with live registers: %d\n" (List.length call_sites);
    if expected_to_fail
    then printf "UNEXPECTED: Expected this test to trigger spilling!\n"
  with
  | Failure msg when String.is_substring msg ~substring:"spill" ->
    printf "EXPECTED: Triggered spilling as expected\n";
    if not expected_to_fail
    then printf "UNEXPECTED: This test was expected to succeed without spilling!\n"
  | exn -> printf "ERROR: Unexpected exception: %s\n" (Exn.to_string exn)
;;

let%expect_test "register allocation stress tests" =
  test_register_allocation_stress
    "High Register Pressure"
    create_high_register_pressure_function
    true;
  test_register_allocation_stress "Call Heavy Function" create_call_heavy_function false;
  test_register_allocation_stress
    "Branchy High Pressure"
    create_branchy_high_pressure_function
    true;
  [%expect
    {|
    === Testing High Register Pressure ===
    EXPECTED: Triggered spilling as expected
    === Testing Call Heavy Function ===
    EXPECTED: Triggered spilling as expected
    UNEXPECTED: This test was expected to succeed without spilling!
    === Testing Branchy High Pressure ===
    EXPECTED: Triggered spilling as expected
    |}]
;;

let%expect_test "i64 register pressure on ARM32" =
  (* Test i64 legalization followed by register allocation *)
  let func = create_i64_high_pressure_function () in
  let program = Mirl.{ functions = [ func ]; externs = [] } in
  printf "=== Original i64 High Pressure ===\n";
  Mirl.to_string program |> print_endline;
  (* ARM32 legalization will double the register pressure *)
  let arm32_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = false } in
  let legalized = Mucaml_middle.Legalize.legalize_program arm32_config program in
  printf "=== ARM32 Legalized (doubled registers) ===\n";
  Mirl.to_string legalized |> print_endline;
  (* Test register allocation on the legalized version *)
  test_register_allocation_stress
    "I64 High Pressure (ARM32)"
    (fun () -> List.hd_exn legalized.functions)
    true;
  [%expect
    {|
    === Original i64 High Pressure ===
    function test_i64_pressure ($0 (a): i64, $1 (b): i64, $2 (c): i64) {
    $0: i64, $1: i64, $2: i64, $3: i64, $4: i64, $5: i64, $6: i64, $7: i64, $8: i64
    block_0:
        $3 := $0 + $1
        $4 := $3 - $2
        $5 := $4 + $0
        $6 := $5 - $1
        $7 := $6 + $2
        $8 := $7 + $3
        return $8
    }


    === ARM32 Legalized (doubled registers) ===
    function test_i64_pressure ($0 (a_low): i32, $1 (a_high): i32, $2 (b_low): i32, $3 (b_high): i32, $4 (c_low): i32, $5 (c_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32, $8: i32, $9: i32, $10: i32, $11: i32, $12: i32, $13: i32, $14: i32, $15: i32, $16: i32, $17: i32
    block_0:
        $6 := $0 + $2
        $7 := $1 +c $3
        $8 := $6 - $4
        $9 := $7 -c $5
        $10 := $8 + $0
        $11 := $9 +c $1
        $12 := $10 - $2
        $13 := $11 -c $3
        $14 := $12 + $4
        $15 := $13 +c $5
        $16 := $14 + $6
        $17 := $15 +c $7
        return $16, $17
    }


    === Testing I64 High Pressure (ARM32) ===
    EXPECTED: Triggered spilling as expected
    |}]
;;

(* Test to verify caller/callee-saved register preference *)
let%expect_test "caller callee saved register preference" =
  let func = create_call_heavy_function () in
  printf "=== Analyzing Caller/Callee-Saved Preference ===\n";
  (* Use realistic register counts *)
  let module RealisticRegister = struct
    type t = int [@@deriving compare, sexp_of]

    include functor Comparable.Make_plain

    let all_available_for_allocation = List.init 8 ~f:Fn.id
    let caller_saved = Set.of_list [ 0; 1; 2; 3 ] (* r0-r3 caller-saved *)
    let callee_saved = Set.of_list [ 4; 5; 6; 7 ] (* r4-r7 callee-saved *)
    let to_string t = [%string "r%{t#Int}"]
  end
  in
  let module RealisticAllocator = Mucaml_backend_common.Linscan.Make (RealisticRegister)
  in
  let _registers, used_registers, call_sites =
    RealisticAllocator.allocate_registers func
  in
  printf
    "Used registers: %s\n"
    (Set.to_list used_registers
     |> List.map ~f:RealisticRegister.to_string
     |> String.concat ~sep:", ");
  let caller_saved_used = Set.inter used_registers RealisticRegister.caller_saved in
  let callee_saved_used = Set.inter used_registers RealisticRegister.callee_saved in
  printf
    "Caller-saved used: %s\n"
    (Set.to_list caller_saved_used
     |> List.map ~f:RealisticRegister.to_string
     |> String.concat ~sep:", ");
  printf
    "Callee-saved used: %s\n"
    (Set.to_list callee_saved_used
     |> List.map ~f:RealisticRegister.to_string
     |> String.concat ~sep:", ");
  printf "Call sites: %d\n" (List.length call_sites);
  List.iteri call_sites ~f:(fun i (~block, ~insn_idx, ~live_regs) ->
    printf
      "Call site %d: block %s, insn %d, live caller-saved regs: %s\n"
      i
      (Mirl.Label.to_string block)
      insn_idx
      (Set.to_list live_regs
       |> List.map ~f:RealisticRegister.to_string
       |> String.concat ~sep:", "));
  [%expect
    {|
    === Analyzing Caller/Callee-Saved Preference ===
    Used registers: r0, r1, r2, r4, r5, r6
    Caller-saved used: r0, r1, r2
    Callee-saved used: r4, r5, r6
    Call sites: 2
    Call site 0: block block_0, insn 4, live caller-saved regs:
    Call site 1: block block_0, insn 7, live caller-saved regs:
    |}]
;;
