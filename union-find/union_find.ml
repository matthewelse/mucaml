open! Core
open! Import

module Id = struct
  type t = I32.t

  let dummy () = -#1l
end

type t = Id.t array

let create () = [||]
