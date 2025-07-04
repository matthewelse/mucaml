open! Core

let test text =
  match Helpers.parse text with
  | Ok ast ->
    let mirl = Mucaml.Mirl.of_ast ast in
    Mucaml.Mirl.to_string mirl |> print_endline
  | Error () -> ()
;;

let%expect_test _ =
  test {| let main _ : i32 = 10 + 32 |};
  [%expect
    {|
    function mucaml_main ($0 (_): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32
    block_0:
        $1 := 10
        $2 := 32
        $3 := $1 + $2
        return $3
    }
    |}];
  test {| let main _ : i32 = 100000 |};
  [%expect
    {|
    function mucaml_main ($0 (_): i32) {
    $0: i32, $1: i32
    block_0:
        $1 := 100000
        return $1
    }
    |}];
  (* Negative constants are just [0 - (abs x)] *)
  test {| let main _ : i32 = (~32) |};
  [%expect
    {|
    function mucaml_main ($0 (_): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32
    block_0:
        $1 := 0
        $2 := 32
        $3 := $1 - $2
        return $3
    }
    |}];
  test
    {|
    external sleep_ms : i32 -> unit = "sleep_ms"
    external led_on : i32 -> unit = "led_on"
    external led_off : i32 -> unit = "led_off"

    let main x : i32 =
      let _ = sleep_ms 100 in
      let _ = led_on 7 in
      let _ = sleep_ms 100 in
      let _ = led_off 7 in
      0     
    |};
  [%expect
    {|
    function mucaml_main ($0 (x): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32, $8: i32, $9: i32
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

    external sleep_ms : i32 -> i32 = "sleep_ms"

    external led_on : i32 -> i32 = "led_on"

    external led_off : i32 -> i32 = "led_off"
    |}];
  (* let bindings *)
  test
    {|
    let main x : i32 =
      let x = 1 + 1 in
      let y = x + 2 in
      let z = y + x in
      z
    |};
  [%expect
    {|
    function mucaml_main ($0 (x): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32
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
    let main x : i32 =
      if x then (3 + 4) else (5 + 6)
    |};
  [%expect
    {|
    function mucaml_main ($0 (x): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32
    block_0:
        branch if $0 to block_1
        jump block_2
    block_1:
        $1 := 3
        $2 := 4
        $3 := $1 + $2
        $7 := $3
        jump block_3
    block_2:
        $4 := 5
        $5 := 6
        $6 := $4 + $5
        $7 := $6
        jump block_3
    block_3:
        return $7
    }
    |}];
  test
    {|
    external f : i32 -> i32 = "f1"

    let main x : i32 =
      let y = f 100 in
      let z = f x in
      let a = f y in
      a + z + y + x
    |};
  [%expect
    {|
    function mucaml_main ($0 (x): i32) {
    $0: i32, $1: i32, $2: i32, $3: i32, $4: i32, $5: i32, $6: i32, $7: i32
    block_0:
        $1 := 100
        $2 := c_call f1($1)
        $3 := c_call f1($0)
        $4 := c_call f1($2)
        $5 := $4 + $3
        $6 := $5 + $2
        $7 := $6 + $0
        return $7
    }

    external f : i32 -> i32 = "f1"
    |}]
;;
