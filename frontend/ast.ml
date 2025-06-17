open! Core

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | Var of string
  | Let of (string * Type.t option) * expr * expr
  | Letrec of (string * Type.t) * expr * expr
  | If of expr * expr * expr
  | Fun of (string * Type.t) list * expr
  | App of expr * expr list
[@@deriving sexp_of]

type t =
  | Function of
      { name : string
      ; params : (string * Type.t) list
      ; body : expr
      }
[@@deriving sexp_of]

let expr_to_string_hum ?indent (expr : expr) =
  let rec aux ?(indent = "") = function
    | Int i -> Printf.sprintf "%s%d" indent i
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
  match t with
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
;;
