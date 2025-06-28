(** [Mirl] is "matt's intermediate representation language", a low-level representation of
    the program that is closer to assembly than the original source code. It is used as an
    intermediate step before generating machine code. *)

open! Core
open! Import

module Label : sig
  type t : immediate [@@deriving equal, sexp_of, to_string]

  val to_int_exn : t -> int

  module For_testing : sig
    val dummy : t
  end
end

module Type : sig
  type t =
    | I32
    | I64
  [@@deriving sexp_of, to_string]
end

module Virtual_register : sig
  type t : immediate [@@deriving sexp_of, to_string]

  include Hashable.S_plain with type t := t
  include Intable.S with type t := t
  include Comparable.S_plain with type t := t

  (* TODO: more efficient set representation (bitsets for small numbers of registers) *)
end

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
    | Add_with_carry of
        { dst : Virtual_register.t
        ; src1 : Virtual_register.t
        ; src2 : Virtual_register.t
        }
    | Sub_with_carry of
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

  val consumes : t -> Virtual_register.t list @ local
  val produces : t -> Virtual_register.t option @ local
end

module Block : sig
  type t = private
    { label : Label.t
    ; instructions : Instruction.t iarray
    ; successors : Label.t iarray
    }
  [@@deriving sexp_of]

  module Builder : sig
    type t

    val push : t @ local -> Instruction.t -> unit
    val push_many : t @ local -> Instruction.t list -> unit
    val label : t @ local -> Label.t
  end

  val to_string : ?indent:string -> t -> string
end

module Register_descriptor : sig
  type t = private { ty : Type.t } [@@deriving sexp_of]
end

module Function : sig
  type t = private
    { name : string
    ; params : (string * Virtual_register.t * Type.t) list
    ; body : Block.t iarray
    ; registers : Register_descriptor.t iarray
    }
  [@@deriving sexp_of, to_string]

  module Builder : sig
    type t

    val fresh_register : t @ local -> ty:Type.t -> Virtual_register.t

    val add_block
      :  t @ local
      -> (Block.Builder.t @ local -> unit) @ local
      -> Block.Builder.t

    val add_block' : t @ local -> (Block.Builder.t @ local -> unit) @ local -> unit
  end

  val build
    :  name:string
    -> params:(string * Type.t) list
    -> (Builder.t @ local -> (string * Virtual_register.t * Type.t) list -> unit) @ local
    -> t
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
