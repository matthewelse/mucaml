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
    | Bool b -> [%string "%{indent}%{b#Bool}"]
    | Unit -> [%string "%{indent}()"]
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
      | Var v -> [%string "%{indent}$%{v#Identifier}"]
      | Let { var; type_; value; body } ->
        let type_ = Option.value_map type_ ~default:"" ~f:Type.to_string in
        let body = aux ~indent:(indent ^ "  ") body in
        [%string "%{indent}let %{var.txt#Identifier}%{type_} = %{aux value} in\n%{body}"]
      | Letrec { var; type_; value; body } ->
        let value = aux value in
        let body = aux ~indent:(indent ^ "  ") body in
        [%string
          "%{indent}letrec %{var.txt#Identifier} : %{type_#Type} = %{value} in\n%{body}"]
      | If { condition; if_true; if_false } ->
        let condition = aux ~indent:(indent ^ "  ") condition in
        let if_true = aux ~indent:(indent ^ "  ") if_true in
        let if_false = aux ~indent:(indent ^ "  ") if_false in
        [%string "%{indent}if %{condition} then\n%{if_true}\nelse\n%{if_false}"]
      | Fun { params; body } ->
        let params_str =
          String.concat
            ~sep:", "
            (List.map params ~f:(fun (name, t) ->
               [%string "%{name.txt#Identifier}: %{t#Type}"]))
        in
        let body = aux ~indent:(indent ^ "  ") body in
        [%string "%{indent}fun ([%{params_str}] -> %{body})"]
      | App { func; args } ->
        let args_str = String.concat ~sep:", " (List.map args ~f:aux) in
        [%string "%{indent}app (%{aux func}, [%{args_str}])"]
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
             [%string "%{name.txt#Identifier} : %{t#Type}"]))
      in
      let body = Expr.to_string_hum ~indent:(indent ^ "  ") body in
      [%string "%{indent}let %{name.txt#Identifier} %{params_str} =\n%{body}"]
    | External { name; type_; c_name; location = _ } ->
      [%string
        "%{indent}external %{name.txt#Identifier} : %{type_#Type} = \"%{c_name.txt}\""]
  ;;
end

type t = Toplevel.t list [@@deriving sexp_of]

let to_string_hum ?(indent = "") t =
  List.map t ~f:(Toplevel.to_string_hum ~indent) |> String.concat ~sep:"\n"
;;
