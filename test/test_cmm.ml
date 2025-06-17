open! Core

let test text =
  match Helpers.parse text with
  | Ok ast ->
    let cmm = Mucaml.Cmm.of_ast ast in
    Mucaml.Cmm.to_string cmm |> print_endline
  | Error () -> ()
;;

let%expect_test "test code gen" =
  test {|
    let main x : int32 =
      (3 + 4)
    |};
  [%expect
    {|
    function mucaml_main (x: int32) {
      $0 := 3
      $1 := 4
      $2 := $0 + $1
      return $2
    }
    |}]
;;
