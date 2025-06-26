open! Core

type t =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | SP
  | LR
  | PC
[@@deriving compare, enumerate, equal, sexp_of]

include functor Comparable.Make_plain

let to_string = function
  | R0 -> "r0"
  | R1 -> "r1"
  | R2 -> "r2"
  | R3 -> "r3"
  | R4 -> "r4"
  | R5 -> "r5"
  | R6 -> "r6"
  | R7 -> "r7"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | SP -> "sp"
  | LR -> "lr"
  | PC -> "pc"
;;

let all_available_for_allocation =
  [ R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; LR ]
;;

let function_args = [: R0; R1; R2; R3 :]
let return_register = R0
let callee_saved = Set.of_list [ R4; R5; R6; R7; R8; R9; R10; R11; R12 ]
