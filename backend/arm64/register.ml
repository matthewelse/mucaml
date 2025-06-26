open! Core

(* TODO: use 64-bit registers -- for the time being, I'm just using 32-bit
   registers to stick close to the existing 32-bit Arm implementation. *)

module T = struct
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
  [@@deriving sexp_of, compare, enumerate, equal, string ~capitalize:"kebab-case"]

  include functor Comparable.Make_plain
end

include T

module Sixty_four = struct
  include T

  let to_string t =
    match t with
    | W0 -> "x0"
    | W1 -> "x1"
    | W2 -> "x2"
    | W3 -> "x3"
    | W4 -> "x4"
    | W5 -> "x5"
    | W6 -> "x6"
    | W7 -> "x7"
    | W8 -> "x8"
    | W9 -> "x9"
    | W10 -> "x10"
    | W11 -> "x11"
    | W12 -> "x12"
    | W13 -> "x13"
    | W14 -> "x14"
    | W15 -> "x15"
    | W16 -> "x16"
    | W17 -> "x17"
    | W18 -> "x18"
    | W19 -> "x19"
    | W20 -> "x20"
    | W21 -> "x21"
    | W22 -> "x22"
    | W23 -> "x23"
    | W24 -> "x24"
    | W25 -> "x25"
    | W26 -> "x26"
    | W27 -> "x27"
    | W28 -> "x28"
    | W29 -> "fp" (* frame pointer *)
    | W30 -> "lr" (* link register *)
  ;;
end

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

let caller_saved =
  Set.of_list
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
    ]
;;

let lr = W30
