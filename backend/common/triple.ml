open! Core

module Architecture = struct
  type t =
    | Arm
    | Arm64
  [@@deriving sexp_of, string]
end

module Vendor = struct
  type t = Unknown [@@deriving sexp_of, string]
end

module Operating_system = struct
  type t =
    | Linux
    | None
  [@@deriving sexp_of, string]
end

module Binary_format = struct
  type t = Elf [@@deriving sexp_of]
end

module Environment = struct
  type t =
    | Eabi
    | Eabihf
  [@@deriving sexp_of, string]
end

type t =
  { architecture : Architecture.t
  ; vendor : Vendor.t
  ; operating_system : Operating_system.t
  ; environment : Environment.t
  ; binary_format : Binary_format.t
  }
[@@deriving sexp_of]

let to_string t =
  [%string
    "%{t.architecture#Architecture}-%{t.vendor#Vendor}-%{t.operating_system#Operating_system}-%{t.environment#Environment}"]
;;

let of_string = function
  | "arm-unknown-none-eabi" ->
    { architecture = Arm
    ; vendor = Unknown
    ; operating_system = None
    ; environment = Eabi
    ; binary_format = Elf
    }
  | "arm-unknown-none-eabihf" ->
    { architecture = Arm
    ; vendor = Unknown
    ; operating_system = None
    ; environment = Eabihf
    ; binary_format = Elf
    }
  | _ -> failwith "Unsupported triple format"
;;

let all_supported =
  [ of_string "arm-unknown-none-eabi"; of_string "arm-unknown-none-eabihf" ]
;;

let arg_type =
  Command.Arg_type.enumerated
    ~case_sensitive:false
    ~list_values_in_help:true
    (module struct
      type nonrec t = t

      let all = all_supported
      let to_string t = to_string t
    end)
;;

let%expect_test "to_string round trips" =
  List.iter all_supported ~f:(fun triple ->
    let str = to_string triple in
    assert (String.equal str (to_string (of_string str)));
    [%expect {| |}])
;;
