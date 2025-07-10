open! Core

type 'a t = ( :: ) of 'a * 'a list

let to_list (hd :: tl) : _ list = hd :: tl
let sexp_of_t sexp_of_a t = [%sexp (to_list t : a list)]
let singleton x = x :: []
let cons x (hd :: tl) = x :: hd :: tl
let map (hd :: tl) ~f = f hd :: List.map tl ~f
let fold (hd :: tl) ~init ~f = List.fold (hd :: tl) ~init ~f
let exists (hd :: tl) ~f = f hd || List.exists tl ~f

let of_list = function
  | [] -> None
  | hd :: tl -> Some (hd :: tl)
;;

let of_list_exn = function
  | [] -> failwith "Tried to create a [Nonempty_list.t] from an empty list."
  | hd :: tl -> hd :: tl
;;
