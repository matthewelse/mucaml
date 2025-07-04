open! Core

type expr =
  | Int32 of int32
  | Int64 of int64
  | Bool of bool
  | Unit
  | Var of string
  | Let of (string * Type.t option) * expr * expr
  | Letrec of (string * Type.t) * expr * expr
  | If of expr * expr * expr
  | Fun of (string * Type.t) list * expr
  | App of expr * expr list
[@@deriving sexp_of]

type toplevel =
  | Function of
      { name : string
      ; params : (string * Type.t) list
      ; body : expr
      }
  | External of
      { name : string
      ; type_ : Type.t
      ; c_name : string
      }
[@@deriving sexp_of]

type t = toplevel list [@@deriving sexp_of]

let expr_to_string_hum ?indent (expr : expr) =
  let rec aux ?(indent = "") = function
    | Int32 i -> Printf.sprintf "%s%ld" indent i
    | Int64 i -> Printf.sprintf "%s%Ld" indent i
    | Bool b -> Printf.sprintf "%s%b" indent b
    | Unit -> Printf.sprintf "%s()" indent
    | Var v -> Printf.sprintf "%s$%s" indent v
    | Let ((v, t), value, body) ->
      Printf.sprintf
        "%slet %s : %s = %s in\n%s"
        indent
        v
        (Option.value_map t ~default:"None" ~f:Type.to_string)
        (aux value)
        (aux ~indent:(indent ^ "  ") body)
    | Letrec ((v, t), value, body) ->
      Printf.sprintf
        "%sletrec %s : %s = %s in\n%s"
        indent
        v
        (Type.to_string t)
        (aux value)
        (aux ~indent:(indent ^ "  ") body)
    | If (cond, then_branch, else_branch) ->
      Printf.sprintf
        "%sif %s then\n%s\nelse\n%s"
        indent
        (aux ~indent:(indent ^ "  ") cond)
        (aux ~indent:(indent ^ "  ") then_branch)
        (aux ~indent:(indent ^ "  ") else_branch)
    | Fun (params, body) ->
      let params_str =
        String.concat
          ~sep:", "
          (List.map params ~f:(fun (name, t) ->
             Printf.sprintf "%s: %s" name (Type.to_string t)))
      in
      Printf.sprintf
        "%sfun ([%s] -> %s)"
        indent
        params_str
        (aux ~indent:(indent ^ "  ") body)
    | App (func, args) ->
      let args_str = String.concat ~sep:", " (List.map args ~f:aux) in
      Printf.sprintf "%sapp (%s, [%s])" indent (aux func) args_str
  in
  aux ?indent expr
;;

let to_string_hum ?(indent = "") t =
  List.map t ~f:(function
    | Function { name; params; body } ->
      let params_str =
        String.concat
          ~sep:", "
          (List.map params ~f:(fun (name, t) ->
             Printf.sprintf "%s : %s" name (Type.to_string t)))
      in
      Printf.sprintf
        "%slet %s %s =\n%s"
        indent
        name
        params_str
        (expr_to_string_hum ~indent:(indent ^ "  ") body)
    | External { name; type_; c_name } ->
      Printf.sprintf
        "%sexternal %s : %s = \"%s\""
        indent
        name
        (Type.to_string type_)
        c_name)
  |> String.concat ~sep:"\n"
;;
