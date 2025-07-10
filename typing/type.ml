open! Ox
open! Import

module Var : sig
  type t : immediate [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val zero : t
  val succ : t -> t
  val to_string : t -> string
end = struct
  type t = int [@@deriving compare, hash, sexp_of]

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain

  let zero = 0
  let succ t = t + 1
  let to_string t = [%string "'_%{t#Int}"]
end

module Base = Mucaml_frontend.Type.Base

type t =
  | Base of Base.t
  | Var of Var.t
  | Fun of t list * t
[@@deriving sexp_of, variants]

let rec of_ast (ty : Mucaml_frontend.Type.t) =
  match ty with
  | Base b -> Base b
  | Fun (args, ret) -> Fun (List.map args ~f:of_ast, of_ast ret)
  | Var _var_name -> 
    (* FIXME: For now, we'll convert frontend type variables to a fresh type variable *)
    (* This is a simplified approach - proper handling would require a var mapping *)
    Var Var.zero
  | Constrained (_constraints, ty) ->
    (* FIXME: For now, we'll ignore constraints and just process the underlying type *)
    (* Proper constraint handling would be implemented in the type checker *)
    of_ast ty
;;

let rec occurs t ~var =
  match t with
  | Base _ -> false
  | Var v -> Var.equal v var
  | Fun (args, res) -> List.exists args ~f:(occurs ~var) || occurs res ~var
;;

let rec subst t ~replacements =
  match t with
  | Base b -> Base b
  | Var v ->
    (match Map.find replacements v with
     | Some t -> t
     | None -> Var v)
  | Fun (args, r) -> Fun (List.map args ~f:(subst ~replacements), subst r ~replacements)
;;

let rec to_string_aux t ~mapping =
  match t with
  | Base base -> Base.to_string base
  | Var v -> Map.find_exn mapping v
  | Fun ([ arg_type ], ret_type) ->
    [%string "%{to_string_aux arg_type ~mapping} -> %{to_string_aux ret_type ~mapping}"]
  | Fun (arg_types, ret_type) ->
    let args_str =
      String.concat ~sep:", " (List.map arg_types ~f:(to_string_aux ~mapping))
    in
    [%string "(%{args_str}) -> %{to_string_aux ret_type ~mapping}"]
;;

let tvs t : Var.Set.t =
  let rec aux t ~acc =
    match t with
    | Base _ -> acc
    | Var v -> Set.add acc v
    | Fun (l, r) ->
      let acc = List.fold ~init:acc ~f:(fun acc ty -> aux ty ~acc) l in
      aux r ~acc
  in
  aux t ~acc:Var.Set.empty
;;

let to_string t =
  let tvs = tvs t in
  let char = ref 0 in
  let mapping =
    Map.of_key_set tvs ~f:(fun _ ->
      let alpha = "abcdefghijklmnopqrstuvwxyz" in
      let i = !char in
      incr char;
      [%string "'%{alpha.[i]#Char}"])
  in
  to_string_aux t ~mapping
;;

module Poly = struct
  type ty = t [@@deriving sexp_of]

  type t =
    { body : ty
    ; quantifiers : Var.t list
    ; constraints : Nothing.t list
    }
  [@@deriving sexp_of]

  let mono body = { body; quantifiers = []; constraints = [] }

  let init { body; quantifiers; _ } ~fresh_var =
    let fresh_vars =
      List.map quantifiers ~f:(fun q -> q, Var (fresh_var ())) |> Var.Map.of_alist_exn
    in
    subst body ~replacements:fresh_vars
  ;;

  let to_string t = to_string t.body
end
