external print : int32 -> unit = "mucaml_print"

let main _ : int32 =
  let _ = print (10 + 32) in
  100
;;
