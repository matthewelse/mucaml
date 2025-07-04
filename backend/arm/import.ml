module Backend_intf = Mucaml_backend_common.Backend_intf
module Mirl = Mucaml_middle.Mirl
module Env = Mucaml_backend_common.Env
module Triple = Mucaml_backend_common.Triple
module Virtual_register = Mirl.Virtual_register

module I32 = struct
  include Stdlib_upstream_compatible.Int32_u

  external ctz
    :  t
    -> (int[@untagged])
    = "unreachable" "caml_int32_ctz_unboxed_to_untagged"

  module O = struct
    let ( lsr ) = shift_right_logical
    let ( land ) = logand
    let ( >= ) = ( >= )
    let ( <= ) = ( <= )
  end
end
