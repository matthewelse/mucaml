open! Core

let test text =
  match Helpers.parse text with
  | Ok ast ->
    let cmm = Mucaml.Mirl.of_ast ast in
    Mucaml.Mirl.to_string cmm |> print_endline
  | Error () -> ()
;;

let%expect_test _ =
  test {| let main _ : int32 = 10 + 32 |};
  [%expect
    {|
    function mucaml_main ($0 (_): int32) {
    block_0:
        $1 := 10
        $2 := 32
        $3 := $1 + $2
        return $3
    }
    |}];
  test {| let main _ : int32 = 100000 |};
  [%expect
    {|
    function mucaml_main ($0 (_): int32) {
    block_0:
        $1 := 100000
        return $1
    }
    |}];
  (* Negative constants are just [0 - (abs x)] *)
  test {| let main _ : int32 = (~32) |};
  [%expect
    {|
    function mucaml_main ($0 (_): int32) {
    block_0:
        $1 := 0
        $2 := 32
        $3 := $1 - $2
        return $3
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
    function mucaml_main ($0 (x): int32) {
    block_0:
        $1 := 100
        $2 := c_call sleep_ms($1)
        $3 := 7
        $4 := c_call led_on($3)
        $5 := 100
        $6 := c_call sleep_ms($5)
        $7 := 7
        $8 := c_call led_off($7)
        $9 := 0
        return $9
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
    function mucaml_main ($0 (x): int32) {
    block_0:
        $1 := 1
        $2 := 1
        $3 := $1 + $2
        $4 := 2
        $5 := $3 + $4
        $6 := $5 + $3
        return $6
    }
    |}];
  test {|
    let main x : int32 =
      if x then (3 + 4) else (5 + 6)
    |};
  [%expect
    {|
    function mucaml_main ($0 (x): int32) {
    block_0:
        branch if $0 to block_2
        jump block_3
    block_1:
        return $1
    block_2:
        $2 := 3
        $3 := 4
        $4 := $2 + $3
        $1 := $4
        jump block_1
    block_3:
        $5 := 5
        $6 := 6
        $7 := $5 + $6
        $1 := $7
        jump block_1
    }
    |}]
;;
