open! Core

let test_simple_i64_add () =
  let open Mucaml_middle.Mirl in
  let func =
    Function.build
      ~name:"test_add"
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
  in
  let program = { functions = [ func ]; externs = [] } in
  print_endline "=== Original MIRL ===";
  to_string program |> print_endline;
  let arm32_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = false } in
  let arm32_legalized = Mucaml_middle.Legalize.legalize_program arm32_config program in
  print_endline "=== ARM32 Legalized MIRL ===";
  to_string arm32_legalized |> print_endline
;;

let%expect_test _ =
  test_simple_i64_add ();
  [%expect
    {|
    === Original MIRL ===
    function test_add ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $2 := $0 + $1
        return $2
    }


    === ARM32 Legalized MIRL ===
    function test_add ($0 (a): i64, $1 (b): i64) {
    $0: i64, $1: i64, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32
    block_0:
        $6 := $2 + $4
        $7 := $3 +c $5
        return $6, $7
    }
    |}]
;;
