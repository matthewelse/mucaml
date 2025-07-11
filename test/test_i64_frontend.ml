open! Core
open! Import

(* Test i64 frontend parsing and integration with legalization *)

let test_i64_parsing text =
  match Helpers.typecheck text with
  | Some ast ->
    let mirl = Mucaml.Mirl.of_ast ast in
    print_endline "=== Original MIRL ===";
    Mucaml.Mirl.to_string mirl |> print_endline;
    (* Test ARM32 legalization *)
    let arm32_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = false } in
    let arm32_legalized = Mucaml_middle.Legalize.legalize_program arm32_config mirl in
    print_endline "=== ARM32 Legalized ===";
    Mucaml.Mirl.to_string arm32_legalized |> print_endline;
    (* Test ARM64 legalization *)
    let arm64_config = Mucaml_middle.Legalize.Config.{ supports_native_i64 = true } in
    let arm64_legalized = Mucaml_middle.Legalize.legalize_program arm64_config mirl in
    print_endline "=== ARM64 Legalized ===";
    Mucaml.Mirl.to_string arm64_legalized |> print_endline
  | None -> ()
;;

let%expect_test "i64 literal parsing and legalization" =
  test_i64_parsing {| let main (_ : i64) = 42L |};
  [%expect
    {|
    === Original MIRL ===


    function mucaml_main ($0 (_): i64) {
    $0: i64, $1: i64
    block_0:
        $1 := 42
        return $1
    }


    === ARM32 Legalized ===


    function mucaml_main ($0 (__low): i32, $1 (__high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32
    block_0:
        $2 := 42
        $3 := 0
        return $2, $3
    }


    === ARM64 Legalized ===


    function mucaml_main ($0 (_): i64) {
    $0: i64, $1: i64
    block_0:
        $1 := 42
        return $1
    }
    |}]
;;

let%expect_test "i64 addition parsing and legalization" =
  test_i64_parsing
    {|external ( + ) : i64 -> i64 -> i64 = "add_i64"
let main (a : i64) = a + 100L |};
  [%expect
    {|
    === Original MIRL ===


    function mucaml_main ($0 (a): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $1 := 100
        $2 := $0 + $1
        return $2
    }

    external + : i64 -> i64 -> i64 = "add_i64"
    === ARM32 Legalized ===


    function mucaml_main ($0 (a_low): i32, $1 (a_high): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32
    block_0:
        $2 := 100
        $3 := 0
        $4 := $0 + $2
        $5 := $1 +c $3
        return $4, $5
    }

    external + : i64 -> i64 -> i64 = "add_i64"
    === ARM64 Legalized ===


    function mucaml_main ($0 (a): i64) {
    $0: i64, $1: i64, $2: i64
    block_0:
        $1 := 100
        $2 := $0 + $1
        return $2
    }

    external + : i64 -> i64 -> i64 = "add_i64"
    |}]
;;

let%expect_test "i32 literal test" =
  test_i64_parsing {| let main (_ : i32) = 42 |};
  [%expect
    {|
    === Original MIRL ===


    function mucaml_main ($0 (_): i32) {
    $0: i32, $1: i32
    block_0:
        $1 := 42
        return $1
    }


    === ARM32 Legalized ===


    function mucaml_main ($0 (_): i32) {
    $0: i32
    block_0:
        $1 := 42
        return $1
    }


    === ARM64 Legalized ===


    function mucaml_main ($0 (_): i32) {
    $0: i32, $1: i32
    block_0:
        $1 := 42
        return $1
    }
    |}]
;;
