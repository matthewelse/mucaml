include Core
module I32 = I32
module I64 = I64

module List = struct
  include List

  let rec iter__local (l @ local) ~(f @ local) =
    match l with
    | [] -> ()
    | x :: xs ->
      f x;
      iter__local xs ~f
  ;;

  let iteri__local (l @ local) ~(f @ local) =
    let rec aux l i ~f =
      match l with
      | [] -> ()
      | x :: xs ->
        f i x;
        aux xs (i + 1) ~f
    in
    aux l 0 ~f
  ;;
end

module Nonempty_list = Nonempty_list

type i32 = I32.t [@@deriving sexp_of]
type i64 = I64.t [@@deriving sexp_of]
