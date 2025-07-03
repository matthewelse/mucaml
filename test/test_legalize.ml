open! Core
open! Import

(* Create MIRL functions manually to test i64 legalization since frontend doesn't support i64 yet *)

let create_i64_add_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_add"
    ~params:[ "a", Type.I64; "b", Type.I64 ]
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

let create_i64_sub_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_sub"
    ~params:[ "a", Type.I64; "b", Type.I64 ]
    (fun builder params ->
      let a, b =
        match params with
        | [ (_, a, _); (_, b, _) ] -> a, b
        | _ -> failwith "Expected 2 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Sub { dst = result; src1 = a; src2 = b }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

let create_i64_immediate_function () =
  let open Mirl in
  Function.build ~name:"test_i64_immediate" ~params:[] (fun builder _params ->
    Function.Builder.add_block' builder (fun block_builder ->
      let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
      let instructions =
        [ Instruction.Set { dst = result; value = 0x12345678 }
        ; (* Large 32-bit value *)
          Instruction.Return [ result ]
        ]
      in
      Block.Builder.push_many block_builder instructions)
    [@nontail])
;;

let create_i64_move_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_move"
    ~params:[ "x", Type.I64 ]
    (fun builder params ->
      let x =
        match params with
        | [ (_, x, _) ] -> x
        | _ -> failwith "Expected 1 parameter"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Mov { dst = result; src = x }; Instruction.Return [ result ] ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

let create_mixed_i32_i64_function () =
  let open Mirl in
  Function.build
    ~name:"test_mixed"
    ~params:[ "a", Type.I32; "b", Type.I64 ]
    (fun builder params ->
      let a, b =
        match params with
        | [ (_, a, _); (_, b, _) ] -> a, b
        | _ -> failwith "Expected 2 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let const42 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Set { dst = const42; value = 42 }
          ; Instruction.Add { dst = temp; src1 = a; src2 = const42 }
          ; Instruction.Add { dst = result; src1 = b; src2 = temp }
          ; (* This will trigger i64+i32 handling *)
            Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

(* Edge case: Large immediate values that span 32-bit boundaries *)
let create_i64_large_immediate_function () =
  let open Mirl in
  Function.build ~name:"test_i64_large_immediate" ~params:[] (fun builder _params ->
    Function.Builder.add_block' builder (fun block_builder ->
      let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
      let instructions =
        [ Instruction.Set { dst = result; value = 0x80000000 }
        ; (* This is Int32.min_value, edge case for sign extension *)
          Instruction.Return [ result ]
        ]
      in
      Block.Builder.push_many block_builder instructions)
    [@nontail])
;;

(* Edge case: Multiple i64 operations in sequence (register pressure) *)
let create_i64_chain_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_chain"
    ~params:[ "x", Type.I64; "y", Type.I64 ]
    (fun builder params ->
      let x, y =
        match params with
        | [ (_, x, _); (_, y, _) ] -> x, y
        | _ -> failwith "Expected 2 parameters"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let temp1 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp2 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let temp3 = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Add { dst = temp1; src1 = x; src2 = y }
          ; Instruction.Sub { dst = temp2; src1 = temp1; src2 = x }
          ; Instruction.Add { dst = temp3; src1 = temp2; src2 = y }
          ; Instruction.Sub { dst = result; src1 = temp3; src2 = temp1 }
          ; Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

(* Edge case: i64 operations with function calls (caller-saved register pressure) *)
let create_i64_with_calls_function () =
  let open Mirl in
  Function.build
    ~name:"test_i64_with_calls"
    ~params:[ "x", Type.I64 ]
    (fun builder params ->
      let x =
        match params with
        | [ (_, x, _) ] -> x
        | _ -> failwith "Expected 1 parameter"
      in
      Function.Builder.add_block' builder (fun block_builder ->
        let const1 = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let call_result = Function.Builder.fresh_register builder ~ty:Type.I32 in
        let temp = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
        let instructions =
          [ Instruction.Set { dst = const1; value = 42 }
          ; Instruction.C_call
              { dst = call_result; func = "external_func"; args = [ const1 ] }
          ; Instruction.Add { dst = temp; src1 = x; src2 = x }
          ; (* x should survive the call *)
            Instruction.Add { dst = result; src1 = temp; src2 = call_result }
          ; (* Mix i64 + i32 after call *)
            Instruction.Return [ result ]
          ]
        in
        Block.Builder.push_many block_builder instructions)
      [@nontail])
;;

(* Edge case: Zero and negative immediate values *)
let create_i64_edge_immediates_function () =
  let open Mirl in
  Function.build ~name:"test_i64_edge_immediates" ~params:[] (fun builder _params ->
    Function.Builder.add_block' builder (fun block_builder ->
      let zero = Function.Builder.fresh_register builder ~ty:Type.I64 in
      let neg_one = Function.Builder.fresh_register builder ~ty:Type.I64 in
      let result = Function.Builder.fresh_register builder ~ty:Type.I64 in
      let instructions =
        [ Instruction.Set { dst = zero; value = 0 }
        ; Instruction.Set { dst = neg_one; value = -1 }
        ; Instruction.Add { dst = result; src1 = zero; src2 = neg_one }
        ; Instruction.Return [ result ]
        ]
      in
      Block.Builder.push_many block_builder instructions)
    [@nontail])
;;

let test_legalization name create_func =
  let func = create_func () in
  let program = Mirl.{ functions = [ func ]; externs = [] } in
  print_endline ("=== Original " ^ name ^ " ===");
  Mirl.to_string program |> print_endline;
  (* Test ARM32 legalization (no native i64) *)
  let arm32_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = false } in
  let arm32_legalized = Mucaml_middle.Legalize.legalize_program arm32_config program in
  print_endline ("=== ARM32 Legalized " ^ name ^ " ===");
  Mirl.to_string arm32_legalized |> print_endline;
  (* Test ARM64 legalization (native i64) *)
  let arm64_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = true } in
  let arm64_legalized = Mucaml_middle.Legalize.legalize_program arm64_config program in
  print_endline ("=== ARM64 Legalized " ^ name ^ " (should be unchanged) ===");
  Mirl.to_string arm64_legalized |> print_endline
;;

(* Test i64 addition legalization *)
let%expect_test "i64 addition legalization" =
  test_legalization "i64 Add" create_i64_add_function;
  [%expect
    {|
    === Original i64 Add ===
    function test_i64_add ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $2 := $0 + $1
        return $2
    }


    === ARM32 Legalized i64 Add ===
    function test_i64_add ($0 (a_low): i32, $1 (a_high): i32, $2 (b_low): i32, $3 (b_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32
    block_0:
        $4 := $0 + $2
        $5 := $1 +c $3
        return $4, $5
    }


    === ARM64 Legalized i64 Add (should be unchanged) ===
    function test_i64_add ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $2 := $0 + $1
        return $2
    }
    |}]
;;

(* Test i64 subtraction legalization *)
let%expect_test "i64 subtraction legalization" =
  test_legalization "i64 Sub" create_i64_sub_function;
  [%expect
    {|
    === Original i64 Sub ===
    function test_i64_sub ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $2 := $0 - $1
        return $2
    }


    === ARM32 Legalized i64 Sub ===
    function test_i64_sub ($0 (a_low): i32, $1 (a_high): i32, $2 (b_low): i32, $3 (b_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32
    block_0:
        $4 := $0 - $2
        $5 := $1 -c $3
        return $4, $5
    }


    === ARM64 Legalized i64 Sub (should be unchanged) ===
    function test_i64_sub ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $2 := $0 - $1
        return $2
    }
    |}]
;;

(* Test i64 immediate value legalization *)
let%expect_test "i64 immediate legalization" =
  test_legalization "i64 Immediate" create_i64_immediate_function;
  [%expect
    {|
    === Original i64 Immediate ===
    function test_i64_immediate () {
    $0: i64
    block_0:
        $0 := 305419896
        return $0
    }


    === ARM32 Legalized i64 Immediate ===
    function test_i64_immediate () {
    $0: i32, $1: i32
    block_0:
        $0 := 305419896
        $1 := 0
        return $0, $1
    }


    === ARM64 Legalized i64 Immediate (should be unchanged) ===
    function test_i64_immediate () {
    $0: i64
    block_0:
        $0 := 305419896
        return $0
    }
    |}]
;;

(* Test i64 move operation legalization *)
let%expect_test "i64 move legalization" =
  test_legalization "i64 Move" create_i64_move_function;
  [%expect
    {|
    === Original i64 Move ===
    function test_i64_move ($0 (x): i64) {
    $0: i64, $1: i64
    block_0:
        $1 := $0
        return $1
    }


    === ARM32 Legalized i64 Move ===
    function test_i64_move ($0 (x_low): i32, $1 (x_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32
    block_0:
        $2 := $0
        $3 := $1
        return $2, $3
    }


    === ARM64 Legalized i64 Move (should be unchanged) ===
    function test_i64_move ($0 (x): i64) {
    $0: i64, $1: i64
    block_0:
        $1 := $0
        return $1
    }
    |}]
;;

(* Test mixed i32/i64 operations *)
let%expect_test "mixed i32 i64 legalization" =
  test_legalization "Mixed i32/i64" create_mixed_i32_i64_function;
  [%expect
    {|
    === Original Mixed i32/i64 ===
    function test_mixed ($0 (a): i32, $1 (b): i64) {
    $0: i32, $1: i64, $2: i32, $3: i32, $4: i64
    block_0:
        $2 := 42
        $3 := $0 + $2
        $4 := $1 + $3
        return $4
    }


    === ARM32 Legalized Mixed i32/i64 ===
    function test_mixed ($0 (a): i32, $1 (b_low): i32, $2 (b_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32
    block_0:
        $2 := 42
        $3 := $0 + $2
        $3 := $1 + $5
        $4 := $2 +c $6
        return $3, $4
    }


    === ARM64 Legalized Mixed i32/i64 (should be unchanged) ===
    function test_mixed ($0 (a): i32, $1 (b): i64) {
    $0: i32, $1: i64, $2: i32, $3: i32, $4: i64
    block_0:
        $2 := 42
        $3 := $0 + $2
        $4 := $1 + $3
        return $4
    }
    |}]
;;

(* Test large immediate values at 32-bit boundary *)
let%expect_test "i64 large immediate legalization" =
  test_legalization "i64 Large Immediate" create_i64_large_immediate_function;
  [%expect
    {|
    === Original i64 Large Immediate ===
    function test_i64_large_immediate () {
    $0: i64
    block_0:
        $0 := 2147483648
        return $0
    }


    === ARM32 Legalized i64 Large Immediate ===
    function test_i64_large_immediate () {
    $0: i32, $1: i32
    block_0:
        $0 := 2147483648
        $1 := 0
        return $0, $1
    }


    === ARM64 Legalized i64 Large Immediate (should be unchanged) ===
    function test_i64_large_immediate () {
    $0: i64
    block_0:
        $0 := 2147483648
        return $0
    }
    |}]
;;

(* Test register pressure with multiple i64 operations *)
let%expect_test "i64 chain operations legalization" =
  test_legalization "i64 Chain" create_i64_chain_function;
  [%expect
    {|
    === Original i64 Chain ===
    function test_i64_chain ($0 (x): i64, $1 (y): i64) {
    $0: i64, $1: i64, $2: i64, $3: i64, $4: i64, $5: i64
    block_0:
        $2 := $0 + $1
        $3 := $2 - $0
        $4 := $3 + $1
        $5 := $4 - $2
        return $5
    }


    === ARM32 Legalized i64 Chain ===
    function test_i64_chain ($0 (x_low): i32, $1 (x_high): i32, $2 (y_low): i32, $3 (y_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32, $8: i32, $9: i32, $10: i32, $11: i32
    block_0:
        $4 := $0 + $2
        $5 := $1 +c $3
        $6 := $4 - $0
        $7 := $5 -c $1
        $8 := $6 + $2
        $9 := $7 +c $3
        $10 := $8 - $4
        $11 := $9 -c $5
        return $10, $11
    }


    === ARM64 Legalized i64 Chain (should be unchanged) ===
    function test_i64_chain ($0 (x): i64, $1 (y): i64) {
    $0: i64, $1: i64, $2: i64, $3: i64, $4: i64, $5: i64
    block_0:
        $2 := $0 + $1
        $3 := $2 - $0
        $4 := $3 + $1
        $5 := $4 - $2
        return $5
    }
    |}]
;;

(* Test i64 operations with function calls *)
let%expect_test "i64 with function calls legalization" =
  test_legalization "i64 with Calls" create_i64_with_calls_function;
  [%expect
    {|
    === Original i64 with Calls ===
    function test_i64_with_calls ($0 (x): i64) {
    $0: i64, $1: i32, $2: i32, $3: i64, $4: i64
    block_0:
        $1 := 42
        $2 := c_call external_func($1)
        $3 := $0 + $0
        $4 := $3 + $2
        return $4
    }


    === ARM32 Legalized i64 with Calls ===
    function test_i64_with_calls ($0 (x_low): i32, $1 (x_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32
    block_0:
        $1 := 42
        $2 := c_call external_func($1)
        $2 := $0 + $0
        $3 := $1 +c $1
        $4 := $2 + $6
        $5 := $3 +c $7
        return $4, $5
    }


    === ARM64 Legalized i64 with Calls (should be unchanged) ===
    function test_i64_with_calls ($0 (x): i64) {
    $0: i64, $1: i32, $2: i32, $3: i64, $4: i64
    block_0:
        $1 := 42
        $2 := c_call external_func($1)
        $3 := $0 + $0
        $4 := $3 + $2
        return $4
    }
    |}]
;;

(* Test edge cases with zero and negative immediates *)
let%expect_test "i64 edge immediate values legalization" =
  test_legalization "i64 Edge Immediates" create_i64_edge_immediates_function;
  [%expect
    {|
    === Original i64 Edge Immediates ===
    function test_i64_edge_immediates () {
    $0: i64, $1: i64, $2: i64
    block_0:
        $0 := 0
        $1 := -1
        $2 := $0 + $1
        return $2
    }


    === ARM32 Legalized i64 Edge Immediates ===
    function test_i64_edge_immediates () {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32
    block_0:
        $0 := 0
        $1 := 0
        $2 := 4294967295
        $3 := -1
        $4 := $0 + $2
        $5 := $1 +c $3
        return $4, $5
    }


    === ARM64 Legalized i64 Edge Immediates (should be unchanged) ===
    function test_i64_edge_immediates () {
    $0: i64, $1: i64, $2: i64
    block_0:
        $0 := 0
        $1 := -1
        $2 := $0 + $1
        return $2
    }
    |}]
;;
