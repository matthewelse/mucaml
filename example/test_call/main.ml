external print_int : i32 -> unit = "mucaml_print"

let main x : i32 =
  let y = x + 5 in
  let z = print_int y in
  z + x

