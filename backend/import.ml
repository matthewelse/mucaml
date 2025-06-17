module Cmm = Mucaml_middle.Cmm

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
