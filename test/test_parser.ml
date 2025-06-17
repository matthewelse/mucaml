open! Core

let test text =
  match Helpers.parse text with
  | Ok ast -> Mucaml.Ast.pprint_prog ast
  | Error () -> ()
;;

let%expect_test "test parser" =
  test {|
    let main x : int32 =
      (3 + 4)
    |};
  [%expect
    {|
    Program
    Function main
    ├── Args
    │   └── x: int32
    └── App
        ├── Var: +
        └── Args:
            ├── Int: 3
            └── Int: 4
    |}]
;;
