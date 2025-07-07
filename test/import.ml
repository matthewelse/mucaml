module Ast = Mucaml_frontend.Ast
module Mirl = Mucaml_middle.Mirl
module Virtual_register = Mirl.Virtual_register

let ident s : Mucaml_frontend.Identifier.t Mucaml_frontend.Located.t =
  { txt = Mucaml_frontend.Identifier.of_string s; loc = Grace.Range.initial }
;;
