external print_int : int32 -> unit = "mucaml_print"

let main x : int32 =
  let y = x + 5 in
  let z = print_int y in
  z + x

