module Backend_intf = Mucaml_backend_common.Backend_intf
module Mirl = Mucaml_middle.Mirl
module Env = Mucaml_backend_common.Env
module Triple = Mucaml_backend_common.Triple
module Virtual_register = Mirl.Virtual_register
open! Core

module I32 = struct
  include Stdlib_upstream_compatible.Int32_u

  external ctz
    :  t
    -> (int[@untagged])
    = "unreachable" "caml_int32_ctz_unboxed_to_untagged"

  module O = struct
    let ( lsr ) = shift_right_logical
    let ( land ) = logand
    let[@inline always] ( >= ) x y = Int32.( >= ) (to_int32 x) (to_int32 y)
    let[@inline always] ( <= ) x y = Int32.( <= ) (to_int32 x) (to_int32 y)
    let[@inline always] ( = ) x y = Int32.( = ) (to_int32 x) (to_int32 y)
  end
end

module I64 = struct
  include Stdlib_upstream_compatible.Int64_u

  module O = struct
    let ( lsr ) = shift_right_logical
    let ( land ) = logand
    let[@inline always] ( >= ) x y = Int64.( >= ) (to_int64 x) (to_int64 y)
    let[@inline always] ( <= ) x y = Int64.( <= ) (to_int64 x) (to_int64 y)
    let[@inline always] ( = ) x y = Int64.( = ) (to_int64 x) (to_int64 y)
  end
end
