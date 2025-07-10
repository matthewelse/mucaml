external print : i32 -> unit = "mucaml_print"
external exit : i32 -> unit = "mucaml_exit"

let main x =
  let value = 
    if x
    then (
      let _ = print 10 in
      100)
    else 99
  in
  exit value
