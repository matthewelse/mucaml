open! Core

let test text =
  match Helpers.parse text with
  | Some ast -> Mucaml.Ast.to_string_hum ast |> print_endline
  | None -> ()
;;

let%expect_test "test parser" =
  test {|
    let main (x : i32) =
      (3 + 4)
    |};
  [%expect {|
    let main (x : i32) =
      app ($+, [3, 4])
    |}];
  test
    {|
    external sleep_ms : i32 -> unit = "sleep_ms"
    external led_on : i32 -> unit = "led_on"
    external led_off : i32 -> unit = "led_off"

    let main (x : i32) =
      let _ = sleep_ms 100 in
      let _ = led_on 7 in
      let _ = sleep_ms 100 in
      let _ = led_off 7 in
      0     
    |};
  [%expect
    {|
    external sleep_ms : i32 -> unit = "sleep_ms"
    external led_on : i32 -> unit = "led_on"
    external led_off : i32 -> unit = "led_off"
    let main (x : i32) =
      let _ = app ($sleep_ms, [100]) in
        let _ = app ($led_on, [7]) in
          let _ = app ($sleep_ms, [100]) in
            let _ = app ($led_off, [7]) in
              0
    |}];
  test {|
    let main (x : bool) =
      if x then (3 + 4) else (5 + 6)
    |};
  [%expect
    {|
    let main (x : bool) =
      if     $x then
        app ($+, [3, 4])
    else
        app ($+, [5, 6])
    |}];
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a b = a + b|};
  [%expect
    {|
    external + : (i32,i32) -> i32 = "add_i32"
    let test (a : _) (b : _) =
      app ($+, [$a, $b])
    |}];
  test {|external ( + ) : (i32, i32) -> i32 = "add_i32"
let test a b : i32 = a + b|};
  [%expect
    {|
    external + : (i32,i32) -> i32 = "add_i32"
    let test (a : _) (b : _) : i32 =
      app ($+, [$a, $b])
    |}]
;;
