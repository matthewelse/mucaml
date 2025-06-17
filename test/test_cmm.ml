open! Core

let test text =
  match Helpers.parse text with
  | Ok ast ->
    let cmm = Mucaml.Cmm.of_ast ast in
    Mucaml.Cmm.to_string cmm |> print_endline
  | Error () -> ()
;;

let%expect_test _ =
  test {| let main _ : int32 = 10 + 32 |};
  [%expect
    {|
    function mucaml_main (_: int32) {
      $0 := 10
      $1 := 32
      $2 := $0 + $1
      return $2
    }
    |}];
  test {| let main _ : int32 = 100000 |};
  [%expect
    {|
    function mucaml_main (_: int32) {
      $0 := 100000
      return $0
    }
    |}];
  (* Negative constants are just [0 - (abs x)] *)
  test {| let main _ : int32 = (~32) |};
  [%expect
    {|
    function mucaml_main (_: int32) {
      $0 := 0
      $1 := 32
      $2 := $0 - $1
      return $2
    }
    |}]
;;
