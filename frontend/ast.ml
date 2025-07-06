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
    { desc : desc
    ; location : Location.t
    }

  and desc =
    | Literal of Literal.t
    | Var of string
    | Let of (string * Type.t option) * t * t
    | Letrec of (string * Type.t) * t * t
    | If of t * t * t
    | Fun of (string * Type.t) list * t
    | App of t * t list
  [@@deriving sexp_of]

  module Desc = struct
    type t = desc [@@deriving sexp_of]
  end

  let to_string_hum ?indent (expr : t) =
    let rec aux ?(indent = "") { desc; _ } =
      match desc with
      | Literal lit -> Literal.to_string_hum ~indent lit
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
