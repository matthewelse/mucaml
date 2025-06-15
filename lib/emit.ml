open! Core

module Function_name =
  String_id.Make
    (struct
      let module_name = "Function_name"
    end)
    ()

module Statement = struct
  type t = Return of int

  let emit t buf =
    let open Arm_dsl in
    match t with
    | Return value ->
      mov_imm buf ~dst:R0 value;
      ret buf
  ;;
end

module Function = struct
  type t =
    { name : string
    ; args : unit
    ; body : Statement.t
    }

  let emit t buf =
    let open Arm_dsl in
    emit_function_prologue buf ~name:t.name;
    Statement.emit t.body buf;
    emit_function_epilogue buf ~name:t.name
  ;;
end

module Program = struct
  type t = Function.t list

  let emit t buf ~target =
    let open Arm_dsl in
    emit_program_prologue buf ~target;
    List.iter t ~f:(fun f -> Function.emit f buf)
  ;;
end
