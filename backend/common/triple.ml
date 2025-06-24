open! Core

module Architecture = struct
  module Arm = struct
    type t = V8m_main [@@deriving equal, sexp_of]

    let to_string = function
      | V8m_main -> "thumbv8m.main"
    ;;

    let of_string = function
      | "thumbv8m.main" -> Ok V8m_main
      | _ -> Error "Unsupported ARM architecture"
    ;;
  end

  type t =
    | Arm of Arm.t
    | Arm64
  [@@deriving equal, sexp_of]

  let to_string = function
    | Arm arm -> Arm.to_string arm
    | Arm64 -> "aarch64"
  ;;

  let of_string = function
    | "thumbv8m.main" -> Ok (Arm Arm.V8m_main)
    | "aarch64" -> Ok Arm64
    | _ -> Error "Unsupported architecture"
  ;;
end

module Vendor = struct
  type t = Unknown [@@deriving equal, sexp_of, string ~capitalize:"kebab-case"]
end

module Operating_system = struct
  type t =
    | Linux
    | None
  [@@deriving equal, sexp_of, string ~capitalize:"kebab-case"]
end

module Binary_format = struct
  type t = Elf [@@deriving equal, sexp_of]
end

module Environment = struct
  type t =
    | Eabi
    | Eabihf
    | Gnu
  [@@deriving equal, sexp_of, string ~capitalize:"kebab-case"]
end

type t =
  { architecture : Architecture.t
  ; vendor : Vendor.t
  ; operating_system : Operating_system.t
  ; environment : Environment.t
  ; binary_format : Binary_format.t
  }
[@@deriving equal, sexp_of]

let to_string = function
  | { architecture = Arm V8m_main
    ; vendor = Unknown
    ; operating_system = None
    ; environment = Eabi
    ; binary_format = Elf
    } -> "thumbv8m.main-none-eabi"
  | { architecture = Arm V8m_main
    ; vendor = Unknown
    ; operating_system = None
    ; environment = Eabihf
    ; binary_format = Elf
    } -> "thumbv8m.main-none-eabihf"
  | { architecture; vendor; operating_system; environment; binary_format = Elf } ->
    [%string
      "%{architecture#Architecture}-%{vendor#Vendor}-%{operating_system#Operating_system}-%{environment#Environment}"]
;;

let of_string
  =
  (* Since the runtime is written in Rust, we match the Rust target triple format. *)
  function
  | "thumbv8m.main-none-eabi" ->
    { architecture = Arm V8m_main
    ; vendor = Unknown
    ; operating_system = None
    ; environment = Eabi
    ; binary_format = Elf
    }
  | "thumbv8m.main-none-eabihf" ->
    { architecture = Arm V8m_main
    ; vendor = Unknown
    ; operating_system = None
    ; environment = Eabihf
    ; binary_format = Elf
    }
  | "aarch64-unknown-linux-gnu" ->
    { architecture = Arm64
    ; vendor = Unknown
    ; operating_system = Linux
    ; environment = Gnu
    ; binary_format = Elf
    }
  | _ -> failwith "Unsupported triple format"
;;

let all_supported =
  [ of_string "thumbv8m.main-none-eabi"
  ; of_string "thumbv8m.main-none-eabihf"
  ; of_string "aarch64-unknown-linux-gnu"
  ]
;;

let default =
  (* TODO: use the host triple if available *)
  of_string "aarch64-unknown-linux-gnu"
;;

let is_host t = equal t default

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
