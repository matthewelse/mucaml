open! Ox
open! Import

module Literal = struct
  type t =
    | Int32 of i32
    | Int64 of i64
    | Bool of bool
    | Unit
    | String of string
  [@@deriving sexp_of]

  let to_string_hum ?(indent = "") = function
    | Int32 i -> [%string "%{indent}%{i#I32}"]
    | Int64 i -> [%string "%{indent}%{i#I64}"]
    | Bool b -> [%string "%{indent}%{b#Bool}"]
    | Unit -> [%string "%{indent}()"]
    | String s -> [%string "%{indent}\"%{s}\""]
  ;;
end

module Expr = struct
  type t =
    { desc : desc
    ; loc : Location.t
    }

  and desc =
    | Literal of Literal.t
    | Var of Identifier.t
    | Let of
        { var : Identifier.t Located.t
        ; type_ : Type.t Located.t option
        ; value : t
        ; body : t
        }
    | Letrec of
        { var : Identifier.t Located.t
        ; type_ : Type.t Located.t option
        ; value : t
        ; body : t
        }
    | If of
        { condition : t
        ; if_true : t
        ; if_false : t
        }
    | Fun of
        { params : (Identifier.t Located.t * Type.t option) Nonempty_list.t
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
        let type_ =
          Option.value_map type_ ~default:"" ~f:(fun t -> Type.to_string t.txt)
        in
        let body = aux ~indent:(indent ^ "  ") body in
        [%string "%{indent}let %{var.txt#Identifier}%{type_} = %{aux value} in\n%{body}"]
      | Letrec { var; type_; value; body } ->
        let value = aux value in
        let body = aux ~indent:(indent ^ "  ") body in
        let type_ =
          Option.value_map type_ ~default:"_" ~f:(fun { txt; _ } -> Type.to_string txt)
        in
        [%string
          "%{indent}letrec %{var.txt#Identifier} : %{type_} = %{value} in\n%{body}"]
      | If { condition; if_true; if_false } ->
        let condition = aux ~indent:(indent ^ "  ") condition in
        let if_true = aux ~indent:(indent ^ "  ") if_true in
        let if_false = aux ~indent:(indent ^ "  ") if_false in
        [%string "%{indent}if %{condition} then\n%{if_true}\nelse\n%{if_false}"]
      | Fun { params; body } ->
        let params_str =
          String.concat
            ~sep:", "
            (List.map (Nonempty_list.to_list params) ~f:(fun (name, ty) ->
               let ty = Option.value_map ty ~default:"_" ~f:Type.to_string in
               [%string "%{name.txt#Identifier}: %{ty}"]))
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
        ; params : (Identifier.t Located.t * Type.t option) Nonempty_list.t
        ; return_type : Type.t Located.t option
        ; body : Expr.t
        ; loc : Location.t
        }
    | External of
        { name : Identifier.t Located.t
        ; type_ : Type.t Located.t
        ; c_name : string Located.t
        ; loc : Location.t
        }
  [@@deriving sexp_of]

  let loc = function
    | Function { loc; _ } | External { loc; _ } -> loc
  ;;

  let to_string_hum ?(indent = "") = function
    | Function { name; params; return_type; body; loc = _ } ->
      let params_str =
        String.concat
          ~sep:" "
          (List.map (Nonempty_list.to_list params) ~f:(fun (name, ty) ->
             let ty = Option.value_map ~f:Type.to_string ~default:"_" ty in
             [%string "(%{name.txt#Identifier} : %{ty})"]))
      in
      let return_type_str =
        Option.value_map return_type ~default:"" ~f:(fun t ->
          [%string " : %{t.txt#Type}"])
      in
      let body = Expr.to_string_hum ~indent:(indent ^ "  ") body in
      [%string
        "%{indent}let %{name.txt#Identifier} %{params_str}%{return_type_str} =\n%{body}"]
    | External { name; type_; c_name; loc = _ } ->
      [%string
        "%{indent}external %{name.txt#Identifier} : %{type_.txt#Type} = \"%{c_name.txt}\""]
  ;;
end

type t = Toplevel.t list [@@deriving sexp_of]

let to_string_hum ?(indent = "") t =
  List.map t ~f:(Toplevel.to_string_hum ~indent) |> String.concat ~sep:"\n"
;;
