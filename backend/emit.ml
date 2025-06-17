open! Core
open! Import

module Function_name =
  String_id.Make
    (struct
      let module_name = "Function_name"
    end)
    ()

module Registers = struct
  type t =
    { mapping : Register.t Cmm.Register.Table.t
    ; mutable next_register : Register.t
    }

  let create () = { mapping = Cmm.Register.Table.create (); next_register = R0 }

  let allocate t reg =
    Hashtbl.find_or_add t.mapping reg ~default:(fun () ->
      (* Allocate a new register and update the next_register pointer *)
      let reg = t.next_register in
      t.next_register <- Register.next reg;
      reg)
  ;;
end

let emit_block (block : Cmm.Block.t) buf ~registers =
  let open Arm_dsl in
  List.iter block.instructions ~f:(fun instruction ->
    match instruction with
    | Add { dest; src1; src2 } ->
      let dest_reg = Registers.allocate registers dest in
      let src1_reg = Registers.allocate registers src1 in
      let src2_reg = Registers.allocate registers src2 in
      add buf ~dst:dest_reg ~src1:src1_reg ~src2:src2_reg
    | Set { dest; value } ->
      let dest_reg = Registers.allocate registers dest in
      mov_imm buf ~dst:dest_reg value
    | Return reg ->
      let reg = Registers.allocate registers reg in
      if not (Register.equal reg R0) then mov buf ~dst:R0 ~src:reg;
      ret buf)
;;

let emit_cmm (program : Cmm.t) =
  let open Arm_dsl in
  let buf = Arm_dsl.create () in
  emit_program_prologue buf;
  List.iter program.functions ~f:(fun func ->
    emit_function_prologue buf ~name:func.name;
    emit_block func.body buf ~registers:(Registers.create ());
    emit_function_epilogue buf ~name:func.name);
  Arm_dsl.to_string buf
;;
