external print_int : i32 -> i32 = "mucaml_print"

let main x =
  let y = x + 5 in
  let z = print_int y in
  z + x

