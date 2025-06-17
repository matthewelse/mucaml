open! Core
open! Import

module Emit = struct
  module type S = sig
    val emit_cmm : Cmm.t -> string
  end
end

module type S = sig
  module Emit : Emit.S
end
