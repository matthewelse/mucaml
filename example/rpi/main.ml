external get_pin : i32 -> i32 = "mucaml_pin"
external toggle_pin : i32 -> i32 = "mucaml_pin_toggle"
external sleep : i32 -> i32 = "mucaml_sleep"

let main _ : i32 =
  let pin = get_pin 0 in
  let _ = toggle_pin pin in
  let _ = sleep 1000 in
  let _ = toggle_pin pin in
  let _ = sleep 1000 in
  let _ = toggle_pin pin in
  let _ = sleep 1000 in
  let _ = toggle_pin pin in
  let _ = sleep 1000 in
  let _ = toggle_pin pin in
  let _ = sleep 1000 in
  let _ = toggle_pin pin in
  let _ = sleep 1000 in
  0

