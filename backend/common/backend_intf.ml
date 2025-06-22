open! Core

module type Target_isa = sig
  module Settings : T

  module Assembly : sig
    type t

    val to_string : t -> string
  end

  val name : string
  val triple : Triple.t
  val build_program : Mucaml_middle.Cmm.t -> Assembly.t Or_error.t
end

module type S = sig
  module Settings : sig
    type t

    val param : t Command.Param.t
  end

  val name : string

  val build_target_isa
    :  Triple.t
    -> Settings.t
    -> (module Target_isa with type Settings.t = Settings.t) Or_error.t
end
