open! Core

(* TODO: use 64-bit registers -- for the time being, I'm just using 32-bit
   registers to stick close to the existing 32-bit Arm implementation. *)

type t =
  | W0
  | W1
  | W2
  | W3
  | W4
  | W5
  | W6
  | W7
  | W8
  | W9
  | W10
  | W11
  | W12
  | W13
  | W14
  | W15
  | W16
  | W17
  | W18
  | W19
  | W20
  | W21
  | W22
  | W23
  | W24
  | W25
  | W26
  | W27
  | W28
  | W29
  | W30
  | W31
[@@deriving sexp_of, compare, enumerate, equal, string ~capitalize:"kebab-case"]

include functor Comparable.Make_plain

let all_available_for_allocation =
  [ W0
  ; W1
  ; W2
  ; W3
  ; W4
  ; W5
  ; W6
  ; W7
  ; W8
  ; W9
  ; W10
  ; W11
  ; W12
  ; W13
  ; W14
  ; W15
  ; W16
  ; W17
  ; W18
  ; W19
  ; W20
  ; W21
  ; W22
  ; W23
  ; W24
  ; W25
  ; W26
  ; W27
  ; W28 (* W29, W30 and W31 are used as fp, lr and sp respectively. *)
  ]
;;

let function_args = [: W0; W1; W2; W3; W4; W5; W6; W7 :]
let return_register = W0
let callee_saved = Set.of_list [ W19; W20; W21; W22; W23; W24; W25; W26; W27; W28 ]
