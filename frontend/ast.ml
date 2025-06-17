open! Core

type name = string [@@deriving sexp_of]

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | Var of name
  | Let of (name * Type.t option) * expr * expr
  | Letrec of (name * Type.t) * expr * expr
  | If of expr * expr * expr
  | Fun of (name * Type.t) * expr
  | App of expr * expr list
[@@deriving sexp_of]

type prog =
  | Function of
      { name : string
      ; params : (name * Type.t) list
      ; body : expr
      }

let rec desugar = function
  | `Fun (params, body) ->
    (match params with
     | (param, ty) :: [] -> Fun ((param, ty), body)
     | (param, ty) :: params -> Fun ((param, ty), desugar (`Fun (params, body)))
     | [] -> Fun (("_", Type.Unit), body))
  | _ -> assert false
;;

let printf ?(indent = "") fmt =
  ksprintf
    (fun result ->
      print_string indent;
      print_string result)
    fmt
;;

let rec pprint_expr ?(prefix = "    ") ~indent = function
  | Int i -> printf ~indent "Int: %d\n" i
  | Bool b -> printf ~indent "Bool: %s\n" (if b then "true" else "false")
  | Unit -> printf ~indent "Unit: ()\n"
  | Var x -> printf ~indent "Var: %s\n" x
  | Let ((x, Some ty), e1, e2) ->
    printf ~indent "Let\n";
    printf ~indent:(prefix ^ "├── ") "%s: %s\n" x (Type.string_of_type ty);
    pprint_expr ~indent:(prefix ^ "├── ") ~prefix:(prefix ^ "│   ") e1;
    pprint_expr ~indent:(prefix ^ "└── ") ~prefix:(prefix ^ "    ") e2
  | Let ((x, None), e1, e2) ->
    printf ~indent "Let\n";
    printf ~indent:(prefix ^ "├── ") "%s\n" x;
    pprint_expr ~indent:(prefix ^ "├── ") ~prefix:(prefix ^ "│   ") e1;
    pprint_expr ~indent:(prefix ^ "└── ") ~prefix:(prefix ^ "    ") e2
  | Letrec ((x, ty), e1, e2) ->
    printf ~indent "Letrec\n";
    printf ~indent:(prefix ^ "├── ") "%s: %s\n" x (Type.string_of_type ty);
    pprint_expr ~indent:(prefix ^ "├── ") ~prefix:(prefix ^ "│   ") e1;
    pprint_expr ~indent:(prefix ^ "└── ") ~prefix:(prefix ^ "    ") e2
  | If (e1, e2, e3) ->
    printf ~indent "If\n";
    pprint_expr ~indent:(prefix ^ "├── ") ~prefix:(prefix ^ "│   ") e1;
    pprint_expr ~indent:(prefix ^ "├── ") ~prefix:(prefix ^ "│   ") e2;
    pprint_expr ~indent:(prefix ^ "└── ") ~prefix:(prefix ^ "    ") e3
  | Fun ((x, ty), e) ->
    printf ~indent "Fun\n";
    printf ~indent:(prefix ^ "├── ") "%s: %s\n" x (Type.string_of_type ty);
    pprint_expr ~indent:(prefix ^ "└── ") ~prefix:(prefix ^ "    ") e
  | App (e1, args) ->
    printf ~indent "App\n";
    pprint_expr ~indent:(prefix ^ "├── ") ~prefix:(prefix ^ "│   ") e1;
    printf ~indent:(prefix ^ "└── ") "Args:\n";
    let prefix = prefix ^ "    " in
    let len = List.length args in
    List.iteri args ~f:(fun i x ->
      let indent = if i = len - 1 then prefix ^ "└── " else prefix ^ "├── " in
      pprint_expr ~indent ~prefix:(prefix ^ "    ") x)
;;

let pprint_params ?(prefix = "│   ") ~indent params =
  printf ~indent "Args\n";
  let len = List.length params in
  List.iteri params ~f:(fun i (x, ty) ->
    let indent = if i = len - 1 then prefix ^ "└── " else prefix ^ "├── " in
    printf ~indent "%s: %s\n" x (Type.string_of_type ty))
;;

let pprint_prog prog =
  print_endline "Program";
  match prog with
  | Function { name; params; body } ->
    printf ~indent:"" "Function %s\n" name;
    pprint_params ~indent:"├── " params;
    pprint_expr ~indent:"└── " body
;;

class ['a] folder =
  object (self)
    method int (acc : 'a) (_ : int) = acc
    method bool (acc : 'a) (_ : bool) = acc
    method unit (acc : 'a) = acc
    method var (acc : 'a) (_ : name) = acc
    method name (acc : 'a) (_ : name) = acc

    method let_ (acc : 'a) ((var, ty), e1, e2) =
      let acc = self#name acc var in
      let acc = Option.value_map ty ~default:acc ~f:(fun ty -> self#type_ acc ty) in
      self#expr e1 (self#expr e2 acc)

    method letrec (acc : 'a) ((name, ty), e1, e2) =
      let acc = self#name acc name in
      let acc = self#type_ acc ty in
      self#expr e1 (self#expr e2 acc)

    method if_ (acc : 'a) (e1, e2, e3) = self#expr e1 (self#expr e2 (self#expr e3 acc))

    method fun_ (acc : 'a) ((var, ty), e) =
      let acc = self#name acc var in
      let acc = self#type_ acc ty in
      self#expr e acc

    method app (acc : 'a) (e1, args) =
      let acc = self#expr e1 acc in
      List.fold args ~init:acc ~f:self#expr

    method type_ (acc : 'a) (_ : Type.t) = acc

    method expr (acc : 'a) (e : expr) =
      match e with
      | Int n -> self#int acc n
      | Bool b -> self#bool acc b
      | Unit -> self#unit acc
      | Var v -> self#var acc v
      | Let (name, value, body) -> self#let_ acc (name, value, body)
      | Letrec (name, value, body) -> self#letrec acc (name, value, body)
      | If (cond, if_true, if_false) -> self#if_ acc (cond, if_true, if_false)
      | Fun (arg, body) -> self#fun_ acc (arg, body)
      | App (func, args) -> self#app acc (func, args)
  end
