open! Core

let test text =
  match Helpers.parse text with
  | Ok ast -> Mucaml.Ast.to_string_hum ast |> print_endline
  | Error () -> ()
;;

let%expect_test "test parser" =
  test {|
    let main x : int32 =
      (3 + 4)
    |};
  [%expect
    {|
    let main x : int32 =
      app ($+, [3, 4])
    |}]
;;
