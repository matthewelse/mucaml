(** [Mirl] is "matt's intermediate representation language", a low-level representation of
    the program that is closer to assembly than the original source code. It is used as an
    intermediate step before generating machine code. *)

open! Core
open! Import

module Label : sig
  type t : immediate [@@deriving sexp_of, string]

  module For_testing : sig
    val dummy : t
  end
end

module Type : sig
  type t = Int32 [@@deriving sexp_of, to_string]
end

module Virtual_register : Unique_id.Id

module Instruction : sig
  type t =
    | Add of
        { dst : Virtual_register.t
        ; src1 : Virtual_register.t
        ; src2 : Virtual_register.t
        }
    | Sub of
        { dst : Virtual_register.t
        ; src1 : Virtual_register.t
        ; src2 : Virtual_register.t
        }
    | Set of
        { dst : Virtual_register.t
        ; value : int
        }
    | Mov of
        { dst : Virtual_register.t
        ; src : Virtual_register.t
        }
    | C_call of
        { dst : Virtual_register.t
        ; func : string
        ; args : Virtual_register.t list
        }
    | Jump of { target : Label.t }
    | Return of Virtual_register.t
    | Branch of
        { condition : Virtual_register.t
        ; target : Label.t
        }
  [@@deriving sexp_of, to_string]
end

module Block : sig
  type t =
    { label : Label.t
    ; instructions : Instruction.t Iarray.t
    }
  [@@deriving sexp_of]

  val to_string : ?indent:string -> t -> string
end

module Function : sig
  type t =
    { name : string
    ; params : (string * Type.t) list
    ; body : Block.t
    }
  [@@deriving sexp_of, to_string]
end

module External : sig
  type t =
    { name : string
    ; arg_types : Type.t list
    ; return_type : Type.t
    ; c_name : string
    }
  [@@deriving sexp_of, to_string]
end

type t =
  { functions : Function.t list
  ; externs : External.t list
  }
[@@deriving sexp_of, to_string]

(** Convert an [Ast.t] to [Mirl.t]. This is the main entry point for converting the
    high-level AST to the low-level MIRL representation. *)
val of_ast : Ast.t -> t
