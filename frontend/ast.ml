open! Ox
module Location = Grace.Range

module Literal = struct
  type t =
    | Int32 of i32
    | Int64 of i64
    | Bool of bool
    | Unit
  [@@deriving sexp_of]

  let to_string_hum ?(indent = "") = function
    | Int32 i -> [%string "%{indent}%{i#I32}"]
    | Int64 i -> [%string "%{indent}%{i#I64}"]
    | Bool b -> Printf.sprintf "%s%b" indent b
    | Unit -> Printf.sprintf "%s()" indent
  ;;
end

module Expr = struct
  type t =
    | Literal of Literal.t * Location.t
    | Var of string * Location.t
    | Let of (string * Type.t option) * t * t * Location.t
    | Letrec of (string * Type.t) * t * t * Location.t
    | If of t * t * t * Location.t
    | Fun of (string * Type.t) list * t * Location.t
    | App of t * t list * Location.t
  [@@deriving sexp_of]

  let location = function
    | Literal (_, loc)
    | Var (_, loc)
    | Let (_, _, _, loc)
    | Letrec (_, _, _, loc)
    | If (_, _, _, loc)
    | Fun (_, _, loc)
    | App (_, _, loc) -> loc
  ;;

  let to_string_hum ?indent (expr : t) =
    let rec aux ?(indent = "") = function
      | Literal (lit, _) -> Literal.to_string_hum ~indent lit
      | Var (v, _) -> Printf.sprintf "%s$%s" indent v
      | Let ((v, t), value, body, _) ->
        Printf.sprintf
          "%slet %s : %s = %s in\n%s"
          indent
          v
          (Option.value_map t ~default:"None" ~f:Type.to_string)
          (aux value)
          (aux ~indent:(indent ^ "  ") body)
      | Letrec ((v, t), value, body, _) ->
        Printf.sprintf
          "%sletrec %s : %s = %s in\n%s"
          indent
          v
          (Type.to_string t)
          (aux value)
          (aux ~indent:(indent ^ "  ") body)
      | If (cond, then_branch, else_branch, _) ->
        Printf.sprintf
          "%sif %s then\n%s\nelse\n%s"
          indent
          (aux ~indent:(indent ^ "  ") cond)
          (aux ~indent:(indent ^ "  ") then_branch)
          (aux ~indent:(indent ^ "  ") else_branch)
      | Fun (params, body, _) ->
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
      | App (func, args, _) ->
        let args_str = String.concat ~sep:", " (List.map args ~f:aux) in
        Printf.sprintf "%sapp (%s, [%s])" indent (aux func) args_str
    in
    aux ?indent expr
  ;;
end

module Toplevel = struct
  type t =
    | Function of
        { name : string
        ; params : (string * Type.t) list
        ; body : Expr.t
        ; location : Location.t
        }
    | External of
        { name : string
        ; type_ : Type.t
        ; c_name : string
        ; location : Location.t
        }
  [@@deriving sexp_of]

  let location = function
    | Function { location; _ } | External { location; _ } -> location
  ;;

  let to_string_hum ?(indent = "") = function
    | Function { name; params; body; location = _ } ->
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
        (Expr.to_string_hum ~indent:(indent ^ "  ") body)
    | External { name; type_; c_name; location = _ } ->
      Printf.sprintf
        "%sexternal %s : %s = \"%s\""
        indent
        name
        (Type.to_string type_)
        c_name
  ;;
end

type t = Toplevel.t list [@@deriving sexp_of]

let to_string_hum ?(indent = "") t =
  List.map t ~f:(Toplevel.to_string_hum ~indent) |> String.concat ~sep:"\n"
;;
