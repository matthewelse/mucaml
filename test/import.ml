module Ast = Mucaml_frontend.Ast
module Mirl = Mucaml_middle.Mirl
module Virtual_register = Mirl.Virtual_register

let ident s : Ast.Identifier.t Ast.Located.t =
  { txt = Ast.Identifier.of_string s; loc = Grace.Range.initial }
;;
