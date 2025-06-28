open! Core

type t

val create : unit -> t
val to_string : t -> string
val emit_program_prologue : t -> unit
val emit_function_prologue : t -> name:string -> unit
val emit_function_epilogue : t -> name:string -> unit
val pop : t -> Register.t list -> unit
val push : t -> Register.t list -> unit
val mov : t -> dst:Register.t -> src:Register.t -> unit
val mov_imm : t -> dst:Register.t -> int32# -> unit
val ret : t -> unit
val add : t -> dst:Register.t -> src1:Register.t -> src2:Register.t -> unit
val sub : t -> dst:Register.t -> src1:Register.t -> src2:Register.t -> unit
val adc : t -> dst:Register.t -> src1:Register.t -> src2:Register.t -> unit
val sbc : t -> dst:Register.t -> src1:Register.t -> src2:Register.t -> unit
val bl : t -> func:string -> unit
val b : t -> target:string -> unit
val label : t -> string -> unit
val cbnz : t -> condition:Register.t -> target:string -> unit
