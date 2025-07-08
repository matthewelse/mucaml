open! Ox
open! Import

module Mutable = struct
  type t = { mutable next_tv : Type.Var.t } [@@deriving sexp_of]

  let empty () = { next_tv = Type.Var.zero }

  let fresh_tv t =
    let tv = t.next_tv in
    t.next_tv <- Type.Var.succ t.next_tv;
    tv
  ;;
end

type t =
  { values : Type.Poly.t Identifier.Map.t @@ global
  ; mut : Mutable.t @@ global
  }
[@@deriving globalize, sexp_of]

let empty () = { values = Identifier.Map.empty; mut = Mutable.empty () }
let fresh_tv t = Mutable.fresh_tv t.mut
let with_var t ~var ~ty = { t with values = Map.set t.values ~key:var ~data:ty }
let var t var = Map.find t.values var
