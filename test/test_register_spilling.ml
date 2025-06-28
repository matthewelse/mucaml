open! Core
open! Import

(* Tests specifically designed to trigger and validate register spilling behavior.
   These tests help identify when the "todo: spill" limitation needs to be addressed. *)

(* Create a function that definitely exceeds register capacity *)
let create_guaranteed_spill_function () =
  let open Mirl in
  Function.build
    ~name:"guaranteed_spill"
    ~params:[]
    (fun builder _params ->
      Function.Builder.add_block' builder (fun block_builder ->
        (* Create 16 registers that are all live at the same time *)
        let regs = Array.init 16 ~f:(fun _ -> 
          Function.Builder.fresh_register builder ~ty:Type.I32) in
        
        (* Initialize all registers *)
        let init_instructions = Array.mapi regs ~f:(fun i reg ->
          Instruction.Set { dst = reg; value = i + 1 }) in
        
        (* Add them all together so they're all live simultaneously *)
        let sum1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let sum2 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let sum3 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let final_sum = Function.Builder.fresh_register builder ~ty:Type.I32 in
        
        let sum_instructions =
          [ Instruction.Add { dst = sum1; src1 = regs.(0); src2 = regs.(1) }
          ; Instruction.Add { dst = sum1; src1 = sum1; src2 = regs.(2) }
          ; Instruction.Add { dst = sum1; src1 = sum1; src2 = regs.(3) }
          ; Instruction.Add { dst = sum1; src1 = sum1; src2 = regs.(4) }
          ; Instruction.Add { dst = sum2; src1 = regs.(5); src2 = regs.(6) }
          ; Instruction.Add { dst = sum2; src1 = sum2; src2 = regs.(7) }
          ; Instruction.Add { dst = sum2; src1 = sum2; src2 = regs.(8) }
          ; Instruction.Add { dst = sum2; src1 = sum2; src2 = regs.(9) }
          ; Instruction.Add { dst = sum3; src1 = regs.(10); src2 = regs.(11) }
          ; Instruction.Add { dst = sum3; src1 = sum3; src2 = regs.(12) }
          ; Instruction.Add { dst = sum3; src1 = sum3; src2 = regs.(13) }
          ; Instruction.Add { dst = sum3; src1 = sum3; src2 = regs.(14) }
          ; Instruction.Add { dst = sum3; src1 = sum3; src2 = regs.(15) }
          ; (* Now all original registers should still be live for the final sum *)
            Instruction.Add { dst = final_sum; src1 = sum1; src2 = sum2 }
          ; Instruction.Add { dst = final_sum; src1 = final_sum; src2 = sum3 }
          ; Instruction.Return [ final_sum ]
          ]
        in
        
        let all_instructions = Array.to_list init_instructions @ sum_instructions in
        Block.Builder.push_many block_builder all_instructions)
      [@nontail])
;;

(* Test with extreme i64 register pressure on ARM32 *)
let create_extreme_i64_spill_function () =
  let open Mirl in
  Function.build
    ~name:"extreme_i64_spill"
    ~params:[]
    (fun builder _params ->
      Function.Builder.add_block' builder (fun block_builder ->
        (* Create 8 i64 registers - on ARM32 this becomes 16 i32 registers *)
        let i64_regs = Array.init 8 ~f:(fun _ -> 
          Function.Builder.fresh_register builder ~ty:Type.I64) in
        
        let init_instructions = Array.mapi i64_regs ~f:(fun i reg ->
          Instruction.Set { dst = reg; value = (i + 1) * 1000 }) in
        
        (* Chain operations to keep all registers live *)
        let temp1 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp2 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        
        let chain_instructions =
          [ Instruction.Add { dst = temp1; src1 = i64_regs.(0); src2 = i64_regs.(1) }
          ; Instruction.Add { dst = temp1; src1 = temp1; src2 = i64_regs.(2) }
          ; Instruction.Add { dst = temp1; src1 = temp1; src2 = i64_regs.(3) }
          ; Instruction.Add { dst = temp2; src1 = i64_regs.(4); src2 = i64_regs.(5) }
          ; Instruction.Add { dst = temp2; src1 = temp2; src2 = i64_regs.(6) }
          ; Instruction.Add { dst = temp2; src1 = temp2; src2 = i64_regs.(7) }
          ; (* All i64_regs should still be live here *)
            Instruction.Add { dst = result; src1 = temp1; src2 = temp2 }
          ; Instruction.Return [ result ]
          ]
        in
        
        let all_instructions = Array.to_list init_instructions @ chain_instructions in
        Block.Builder.push_many block_builder all_instructions)
      [@nontail])
;;

(* Test register pressure across multiple function calls *)
let create_call_spill_function () =
  let open Mirl in
  Function.build
    ~name:"call_spill"
    ~params:[]
    (fun builder _params ->
      Function.Builder.add_block' builder (fun block_builder ->
        (* Create many registers that need to survive multiple calls *)
        let vars = Array.init 12 ~f:(fun _ -> 
          Function.Builder.fresh_register builder ~ty:Type.I32) in
        
        let call_results = Array.init 3 ~f:(fun _ -> 
          Function.Builder.fresh_register builder ~ty:Type.I32) in
        
        let init_instructions = Array.mapi vars ~f:(fun i reg ->
          Instruction.Set { dst = reg; value = i * 10 }) in
        
        let call_instructions =
          [ (* First call - many vars should be live across it *)
            Instruction.C_call { dst = call_results.(0); func = "func1"; args = [ vars.(0) ] }
          ; Instruction.Add { dst = vars.(1); src1 = vars.(1); src2 = call_results.(0) }
          ; (* Second call - even more pressure *)
            Instruction.C_call { dst = call_results.(1); func = "func2"; args = [ vars.(2) ] }
          ; Instruction.Add { dst = vars.(3); src1 = vars.(3); src2 = call_results.(1) }
          ; (* Third call - maximum pressure *)
            Instruction.C_call { dst = call_results.(2); func = "func3"; args = [ vars.(4) ] }
          ; (* Now use all remaining vars to keep them live *)
            Instruction.Add { dst = vars.(5); src1 = vars.(5); src2 = vars.(6) }
          ; Instruction.Add { dst = vars.(7); src1 = vars.(7); src2 = vars.(8) }
          ; Instruction.Add { dst = vars.(9); src1 = vars.(9); src2 = vars.(10) }
          ; Instruction.Add { dst = vars.(11); src1 = vars.(11); src2 = call_results.(2) }
          ; Instruction.Return [ vars.(11) ]
          ]
        in
        
        let all_instructions = Array.to_list init_instructions @ call_instructions in
        Block.Builder.push_many block_builder all_instructions)
      [@nontail])
;;

(* Analyze spilling requirements *)
let analyze_spilling_requirements func_name create_func =
  printf "=== Analyzing %s ===\n" func_name;
  let func = create_func () in
  
  (* Count virtual registers *)
  let num_virtual_regs = Iarray.length func.Mirl.Function.registers in
  printf "Virtual registers needed: %d\n" num_virtual_regs;
  
  (* Analyze register types *)
  let i32_count = ref 0 in
  let i64_count = ref 0 in
  Iarray.iter func.Mirl.Function.registers ~f:(fun { ty } ->
    match ty with
    | Mirl.Type.I32 -> incr i32_count
    | Mirl.Type.I64 -> incr i64_count);
  
  printf "i32 registers: %d, i64 registers: %d\n" !i32_count !i64_count;
  
  (* Test with different register capacities *)
  let test_capacity capacity =
    printf "Testing with %d physical registers: " capacity;
    let module TestReg = struct
      type t = int [@@deriving compare, sexp_of]
      include functor Comparable.Make_plain
      let all_available_for_allocation = List.init capacity ~f:Fn.id
      let caller_saved = Set.of_list (List.init (capacity / 2) ~f:Fn.id)
      let callee_saved = Set.of_list (List.init (capacity / 2) ~f:(fun i -> i + (capacity / 2)))
    end in
    let module TestAlloc = Mucaml_backend_common.Linscan.Make (TestReg) in
    try
      let _, used, _ = TestAlloc.allocate_registers func in
      printf "SUCCESS (used %d/%d)\n" (Set.length used) capacity
    with
    | Failure msg when String.is_substring msg ~substring:"spill" -> printf "SPILL REQUIRED\n"
    | exn -> printf "ERROR: %s\n" (Exn.to_string exn)
  in
  
  List.iter [ 4; 6; 8; 12; 16 ] ~f:test_capacity;
  printf "\n"
;;

let%expect_test "spilling requirement analysis" =
  analyze_spilling_requirements "Guaranteed Spill" create_guaranteed_spill_function;
  analyze_spilling_requirements "Call Spill" create_call_spill_function;
  [%expect {|
    === Analyzing Guaranteed Spill ===
    Virtual registers needed: 20
    i32 registers: 20, i64 registers: 0
    Testing with 4 physical registers: SPILL REQUIRED
    Testing with 6 physical registers: SPILL REQUIRED
    Testing with 8 physical registers: SPILL REQUIRED
    Testing with 12 physical registers: SPILL REQUIRED
    Testing with 16 physical registers: SUCCESS (used 16/16)

    === Analyzing Call Spill ===
    Virtual registers needed: 15
    i32 registers: 15, i64 registers: 0
    Testing with 4 physical registers: SPILL REQUIRED
    Testing with 6 physical registers: SPILL REQUIRED
    Testing with 8 physical registers: SPILL REQUIRED
    Testing with 12 physical registers: SUCCESS (used 12/12)
    Testing with 16 physical registers: SUCCESS (used 12/16)
    |}]
;;

let%expect_test "i64 extreme spilling on ARM32" =
  let func = create_extreme_i64_spill_function () in
  let program = Mirl.{ functions = [ func ]; externs = [] } in
  
  printf "=== Before ARM32 legalization ===\n";
  analyze_spilling_requirements "Extreme i64" (fun () -> func);
  
  (* Apply ARM32 legalization *)
  let arm32_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = false } in
  let legalized = Mucaml_middle.Legalize.legalize_program arm32_config program in
  let legalized_func = List.hd_exn legalized.functions in
  
  printf "=== After ARM32 legalization ===\n";
  analyze_spilling_requirements "Extreme i64 (ARM32)" (fun () -> legalized_func);
  [%expect {|
    === Before ARM32 legalization ===
    === Analyzing Extreme i64 ===
    Virtual registers needed: 11
    i32 registers: 0, i64 registers: 11
    Testing with 4 physical registers: SPILL REQUIRED
    Testing with 6 physical registers: SPILL REQUIRED
    Testing with 8 physical registers: SUCCESS (used 8/8)
    Testing with 12 physical registers: SUCCESS (used 8/12)
    Testing with 16 physical registers: SUCCESS (used 8/16)

    === After ARM32 legalization ===
    === Analyzing Extreme i64 (ARM32) ===
    Virtual registers needed: 22
    i32 registers: 22, i64 registers: 0
    Testing with 4 physical registers: SPILL REQUIRED
    Testing with 6 physical registers: SPILL REQUIRED
    Testing with 8 physical registers: SPILL REQUIRED
    Testing with 12 physical registers: SPILL REQUIRED
    Testing with 16 physical registers: SUCCESS (used 16/16)
    |}]
;;

(* Demonstrate the current spilling limitation *)
let%expect_test "current spilling limitation demonstration" =
  printf "=== Current Spilling Limitation ===\n";
  printf "The register allocator currently fails with 'todo: spill' when register pressure exceeds capacity.\n";
  printf "This affects:\n";
  printf "1. Complex functions with many local variables\n";
  printf "2. i64 operations on ARM32 (doubles register usage)\n";
  printf "3. Functions with many live variables across calls\n";
  printf "4. Deeply nested expressions\n";
  printf "\n";
  printf "To fix this, the register allocator needs:\n";
  printf "1. Spill location allocation (stack slots)\n";
  printf "2. Spill/reload instruction insertion\n";
  printf "3. Live range splitting for better allocation\n";
  printf "4. Better heuristics for spill candidate selection\n";
  [%expect {|
    === Current Spilling Limitation ===
    The register allocator currently fails with 'todo: spill' when register pressure exceeds capacity.
    This affects:
    1. Complex functions with many local variables
    2. i64 operations on ARM32 (doubles register usage)
    3. Functions with many live variables across calls
    4. Deeply nested expressions

    To fix this, the register allocator needs:
    1. Spill location allocation (stack slots)
    2. Spill/reload instruction insertion
    3. Live range splitting for better allocation
    4. Better heuristics for spill candidate selection |}]
;;