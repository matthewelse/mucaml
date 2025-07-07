open! Ox
module Location = Grace.Range

module Located = struct
  type 'a t =
    { txt : 'a
    ; loc : Location.t
    }
  [@@deriving sexp_of]
end

module Identifier =
  String_id.Make
    (struct
      let module_name = "Ast.Identifier"
    end)
    ()

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
    | Var of Identifier.t
    | Let of
        { var : Identifier.t Located.t
        ; type_ : Type.t option
        ; value : t
        ; body : t
        }
    | Letrec of
        { var : Identifier.t Located.t
        ; type_ : Type.t
        ; value : t
        ; body : t
        }
    | If of
        { condition : t
        ; if_true : t
        ; if_false : t
        }
    | Fun of
        { params : (Identifier.t Located.t * Type.t) list
        ; body : t
        }
    | App of
        { func : t
        ; args : t list
        }
  [@@deriving sexp_of]

  module Desc = struct
    type t = desc [@@deriving sexp_of]
  end

  let to_string_hum ?indent (expr : t) =
    let rec aux ?(indent = "") { desc; _ } =
      match desc with
      | Literal lit -> Literal.to_string_hum ~indent lit
      | Var v -> Printf.sprintf "%s$%s" indent (Identifier.to_string v)
      | Let { var; type_; value; body } ->
        Printf.sprintf
          "%slet %s : %s = %s in\n%s"
          indent
          (Identifier.to_string var.txt)
          (Option.value_map type_ ~default:"None" ~f:Type.to_string)
          (aux value)
          (aux ~indent:(indent ^ "  ") body)
      | Letrec { var; type_; value; body } ->
        Printf.sprintf
          "%sletrec %s : %s = %s in\n%s"
          indent
          (Identifier.to_string var.txt)
          (Type.to_string type_)
          (aux value)
          (aux ~indent:(indent ^ "  ") body)
      | If { condition; if_true; if_false } ->
        Printf.sprintf
          "%sif %s then\n%s\nelse\n%s"
          indent
          (aux ~indent:(indent ^ "  ") condition)
          (aux ~indent:(indent ^ "  ") if_true)
          (aux ~indent:(indent ^ "  ") if_false)
      | Fun { params; body } ->
        let params_str =
          String.concat
            ~sep:", "
            (List.map params ~f:(fun (name, t) ->
               Printf.sprintf "%s: %s" (Identifier.to_string name.txt) (Type.to_string t)))
        in
        Printf.sprintf
          "%sfun ([%s] -> %s)"
          indent
          params_str
          (aux ~indent:(indent ^ "  ") body)
      | App { func; args } ->
        let args_str = String.concat ~sep:", " (List.map args ~f:aux) in
        Printf.sprintf "%sapp (%s, [%s])" indent (aux func) args_str
    in
    aux ?indent expr
  ;;
end

module Toplevel = struct
  type t =
    | Function of
        { name : Identifier.t Located.t
        ; params : (Identifier.t Located.t * Type.t) list
        ; body : Expr.t
        ; location : Location.t
        }
    | External of
        { name : Identifier.t Located.t
        ; type_ : Type.t
        ; c_name : string Located.t
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
             Printf.sprintf "%s : %s" (Identifier.to_string name.txt) (Type.to_string t)))
      in
      Printf.sprintf
        "%slet %s %s =\n%s"
        indent
        (Identifier.to_string name.txt)
        params_str
        (Expr.to_string_hum ~indent:(indent ^ "  ") body)
    | External { name; type_; c_name; location = _ } ->
      Printf.sprintf
        "%sexternal %s : %s = \"%s\""
        indent
        (Identifier.to_string name.txt)
        (Type.to_string type_)
        c_name.txt
  ;;
end

type t = Toplevel.t list [@@deriving sexp_of]

let to_string_hum ?(indent = "") t =
  List.map t ~f:(Toplevel.to_string_hum ~indent) |> String.concat ~sep:"\n"
;;
