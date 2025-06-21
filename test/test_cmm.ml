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
    |}];
  test
    {|
    external sleep_ms : int32 -> unit = "sleep_ms"
    external led_on : int32 -> unit = "led_on"
    external led_off : int32 -> unit = "led_off"

    let main x : int32 =
      let _ = sleep_ms 100 in
      let _ = led_on 7 in
      let _ = sleep_ms 100 in
      let _ = led_off 7 in
      0     
    |};
  [%expect
    {|
    function mucaml_main (x: int32) {
      $0 := 100
      $1 := c_call sleep_ms($0)
      $2 := 7
      $3 := c_call led_on($2)
      $4 := 100
      $5 := c_call sleep_ms($4)
      $6 := 7
      $7 := c_call led_off($6)
      $8 := 0
      return $8
    }

    external sleep_ms : int32 -> int32 = "sleep_ms"

    external led_on : int32 -> int32 = "led_on"

    external led_off : int32 -> int32 = "led_off"
    |}];
  (* let bindings *)
  test
    {|
    let main x : int32 =
      let x = 1 + 1 in
      let y = x + 2 in
      let z = y + x in
      z
    |};
  [%expect
    {|
    function mucaml_main (x: int32) {
      $0 := 1
      $1 := 1
      $2 := $0 + $1
      $3 := 2
      $4 := $2 + $3
      $5 := $4 + $2
      return $5
    }
    |}]
;;
