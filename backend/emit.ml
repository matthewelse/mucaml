open! Core
open! Import

module Registers = struct
  type t =
    { mapping : Register.t Cmm.Register.Table.t
    ; used : Register.Set.t
    }

  let find_exn t register = Hashtbl.find_exn t.mapping register
  let find t register = Hashtbl.find t.mapping register

  let colour ~precoloured (interference : Cmm.Register.Set.t Cmm.Register.Table.t) =
    (* This is a very crappy register allocation algorithm. It goes through the registers
       left to allocate one-by-one, and checks whether there are any available registers
       that don't interfere with it, and picks an arbitrary one.

       If no non-interfering physical registers are available, we raise lol.

       At some point, I'm going to need to spill things to the stack, but I'm too lazy to
       do that now, and we'll cross that bridge when we get to it. *)
    let mapping = Cmm.Register.Table.create () in
    let available_registers =
      Register.Set.of_list Register.all_available_for_allocation
    in
    let rec loop () =
      match Hashtbl.choose interference with
      | None ->
        (* Done! *)
        ()
      | Some (virtual_register, interfering_regs) ->
        let available_registers =
          Set.fold interfering_regs ~init:available_registers ~f:(fun acc virtual_reg ->
            match Hashtbl.find mapping virtual_reg with
            | None -> acc
            | Some mapping -> Set.remove acc mapping)
        in
        let phys_register =
          match Hashtbl.find precoloured virtual_register with
          | Some reg ->
            if Set.mem available_registers reg
            then reg
            else
              failwithf
                "Register allocation failed: precoloured register %s is not available"
                (Register.to_string reg)
                ()
          | None -> Set.min_elt_exn available_registers
        in
        Hashtbl.set mapping ~key:virtual_register ~data:phys_register;
        Hashtbl.remove interference virtual_register;
        loop ()
    in
    loop ();
    mapping
  ;;

  let allocate (func : Cmm.Function.t) =
    let interference : Cmm.Register.Set.t Cmm.Register.Table.t =
      Cmm.Register.Table.create ()
    in
    let interfere x y =
      if not (Cmm.Register.equal x y)
      then (
        Hashtbl.update interference x ~f:(function
          | None -> Cmm.Register.Set.singleton y
          | Some vars -> Set.add vars y);
        Hashtbl.update interference y ~f:(function
          | None -> Cmm.Register.Set.singleton x
          | Some vars -> Set.add vars x))
      else
        Hashtbl.update interference x ~f:(function
          | None -> Cmm.Register.Set.empty
          | Some x -> x)
    in
    let block = func.body in
    let precoloured = Cmm.Register.Table.create () in
    let rec loop idx ~live_variables =
      match idx >= 0 && idx < Iarray.length block.instructions with
      | false -> ()
      | true ->
        let (instruction : Cmm.Instruction.t) = Iarray.get block.instructions idx in
        let dest =
          match instruction with
          | Add { dest; _ }
          | Sub { dest; _ }
          | Set { dest; _ }
          | C_call { dest; _ }
          | Mov { dest; _ } -> dest
        in
        (* [live_variables] are "live-out", so interfere with [dest]. *)
        Set.iter live_variables ~f:(fun var -> interfere dest var);
        let consumes acc ~reg = Set.add acc reg in
        let assigns acc ~reg = Set.remove acc reg in
        let live_variables =
          match instruction with
          | Add { dest; src1; src2 } | Sub { dest; src1; src2 } ->
            assigns live_variables ~reg:dest |> consumes ~reg:src1 |> consumes ~reg:src2
          | Set { dest; value = _ } -> assigns live_variables ~reg:dest
          | C_call { dest; func = _; args } ->
            let live_variables = assigns live_variables ~reg:dest in
            List.iteri args ~f:(fun i arg ->
              let reg = Iarray.get Register.function_args i in
              Hashtbl.set precoloured ~key:arg ~data:reg);
            Hashtbl.set precoloured ~key:dest ~data:Register.return_register;
            List.fold args ~init:live_variables ~f:(fun acc reg -> consumes acc ~reg)
          | Mov { dest; src } ->
            let live_variables = assigns live_variables ~reg:dest in
            consumes live_variables ~reg:src
        in
        loop (idx - 1) ~live_variables
    in
    let live_variables =
      match block.terminator with
      | Return reg ->
        Hashtbl.set precoloured ~key:reg ~data:Register.return_register;
        Hashtbl.set interference ~key:reg ~data:Cmm.Register.Set.empty;
        Cmm.Register.Set.singleton reg
    in
    loop (Iarray.length block.instructions - 1) ~live_variables;
    let mapping = colour interference ~precoloured in
    { mapping; used = Hashtbl.data mapping |> Register.Set.of_list }
  ;;
end

let c_call buf ~dst ~func ~args =
  let open Arm_dsl in
  List.iteri args ~f:(fun i arg ->
    let reg = Iarray.get Register.function_args i in
    if not (Register.equal reg arg) then mov buf ~dst:reg ~src:arg);
  bl buf ~func;
  match dst with
  | None -> ()
  | Some dst ->
    if not (Register.equal dst Register.return_register)
    then mov buf ~dst ~src:Register.return_register
;;

let emit_block (block : Cmm.Block.t) buf ~registers =
  let open Arm_dsl in
  Iarray.iter block.instructions ~f:(fun instruction ->
    match instruction with
    | Add { dest; src1; src2 } ->
      let dest_reg = Registers.find_exn registers dest in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      add buf ~dst:dest_reg ~src1:src1_reg ~src2:src2_reg
    | Sub { dest; src1; src2 } ->
      let dest_reg = Registers.find_exn registers dest in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      sub buf ~dst:dest_reg ~src1:src1_reg ~src2:src2_reg
    | Set { dest; value } ->
      let dest_reg = Registers.find_exn registers dest in
      mov_imm buf ~dst:dest_reg (I32.of_int value)
    | Mov { dest; src } ->
      let dest_reg = Registers.find_exn registers dest in
      let src_reg = Registers.find_exn registers src in
      mov buf ~dst:dest_reg ~src:src_reg
    | C_call { dest; func; args } ->
      let dest_reg = Registers.find registers dest in
      let args_regs = List.map args ~f:(Registers.find_exn registers) in
      c_call buf ~dst:dest_reg ~func ~args:args_regs);
  match block.terminator with
  | Return reg ->
    let reg = Registers.find_exn registers reg in
    if not (Register.equal reg R0) then mov buf ~dst:R0 ~src:reg;
    ret buf
;;

let emit_function (func : Cmm.Function.t) buf =
  let open Arm_dsl in
  let registers = Registers.allocate func in
  let clobbered_callee_saved_registers =
    Set.inter registers.used Register.callee_saved |> Set.to_list
  in
  emit_function_prologue buf ~name:func.name;
  if not (List.is_empty clobbered_callee_saved_registers)
  then push buf clobbered_callee_saved_registers;
  emit_block func.body buf ~registers;
  if not (List.is_empty clobbered_callee_saved_registers)
  then pop buf clobbered_callee_saved_registers;
  emit_function_epilogue buf ~name:func.name
;;

let emit_cmm_without_prologue (program : Cmm.t) buf =
  List.iter program.functions ~f:(fun func -> emit_function func buf);
  Arm_dsl.to_string buf
;;

let emit_cmm (program : Cmm.t) =
  let open Arm_dsl in
  let buf = Arm_dsl.create () in
  emit_program_prologue buf;
  emit_cmm_without_prologue program buf
;;

module For_testing = struct
  let emit_cmm_without_prologue program =
    let buf = Arm_dsl.create () in
    emit_cmm_without_prologue program buf
  ;;
end
