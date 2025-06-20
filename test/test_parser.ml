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
  [%expect {|
    let main x : int32 =
      app ($+, [3, 4])
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
      let _ = sleep_ms 100 in
      let _ = led_on 7 in
      let _ = sleep_ms 100 in
      let _ = led_off 7 in
      let _ = sleep_ms 100 in
      let _ = led_on 7 in
      let _ = sleep_ms 100 in
      let _ = led_off 7 in
      0     
    |};
  [%expect
    {|
    external sleep_ms : int32 -> unit = "sleep_ms"
    external led_on : int32 -> unit = "led_on"
    external led_off : int32 -> unit = "led_off"
    let main x : int32 =
      let _ : None = app ($sleep_ms, [100]) in
        let _ : None = app ($led_on, [7]) in
          let _ : None = app ($sleep_ms, [100]) in
            let _ : None = app ($led_off, [7]) in
              let _ : None = app ($sleep_ms, [100]) in
                let _ : None = app ($led_on, [7]) in
                  let _ : None = app ($sleep_ms, [100]) in
                    let _ : None = app ($led_off, [7]) in
                      let _ : None = app ($sleep_ms, [100]) in
                        let _ : None = app ($led_on, [7]) in
                          let _ : None = app ($sleep_ms, [100]) in
                            let _ : None = app ($led_off, [7]) in
                              0
    |}]
;;
