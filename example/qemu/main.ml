external print : int32 -> unit = "mucaml_print"

let main x : int32 =
  if x
  then (
    let _ = print 10 in
    100)
  else 99
