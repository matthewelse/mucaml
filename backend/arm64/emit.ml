open! Core
open! Import
module Linscan = Mucaml_backend_common.Linscan.Make (Register)

module Registers = struct
  type t =
    { mapping : Register.t Virtual_register.Table.t
    ; used : Register.Set.t
    ; call_sites : (block:Mirl.Label.t * insn_idx:int * live_regs:Register.Set.t) list
    ; register_types : Mirl.Type.t Virtual_register.Table.t
    }

  let find_exn t register = Hashtbl.find_exn t.mapping register
  let find t register = Hashtbl.find t.mapping register
  let get_type_exn t register = Hashtbl.find_exn t.register_types register

  let is_i64 t register =
    match Hashtbl.find t.register_types register with
    | Some Mirl.Type.I64 -> true
    | Some Mirl.Type.I32 | None -> false
  ;;
end

let label_name ~function_name ~label = [%string "%{function_name}__%{label#Mirl.Label}"]

let rec iteri__local (l @ local) ~f =
  match l with
  | [] -> ()
  | x :: xs ->
    f x;
    iteri__local xs ~f
;;

let c_call buf ~dst ~func ~args ~clobbered_caller_saved_registers =
  let open Arm_dsl in
  if not (List.is_empty clobbered_caller_saved_registers)
  then push buf clobbered_caller_saved_registers;
  let rec iteri__local (l @ local) ~f =
    match l with
    | [] -> ()
    | x :: xs ->
      f x;
      iteri__local xs ~f
  in
  iteri__local
    (Mucaml_backend_common.Parallel_move.parallel_move
       ~src:(Array.of_list args)
       ~dst:
         (List.mapi args ~f:(fun i _ -> Iarray.get Register.function_args i)
          |> Iarray.of_list)
       ~tmp:(fun _ -> W12))
    ~f:(fun (dst, src) -> mov buf ~dst ~src);
  bl buf ~func;
  (match dst with
   | None -> ()
   | Some dst ->
     if not (Register.equal dst Register.return_register)
     then mov buf ~dst ~src:Register.return_register);
  if not (List.is_empty clobbered_caller_saved_registers)
  then pop buf clobbered_caller_saved_registers
;;

let emit_block
  (block : Mirl.Block.t)
  buf
  ~registers
  ~function_name
  ~clobbered_callee_saved_registers
  =
  let open Arm_dsl in
  label buf (label_name ~function_name ~label:block.label);
  Iarray.iteri block.instructions ~f:(fun insn_idx instruction ->
    match instruction with
    | Add { dst; src1; src2 } ->
      let dst_reg = Registers.find_exn registers dst in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      if Registers.is_i64 registers dst
      then add_i64 buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
      else add buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
    | Sub { dst; src1; src2 } ->
      let dst_reg = Registers.find_exn registers dst in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      if Registers.is_i64 registers dst
      then sub_i64 buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
      else sub buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
    | Add_with_carry { dst; src1; src2 } ->
      (* ARM64 doesn't need these since it has native i64 support,
         but we handle them for completeness - treat as regular add *)
      let dst_reg = Registers.find_exn registers dst in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      add buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
    | Sub_with_carry { dst; src1; src2 } ->
      (* ARM64 doesn't need these since it has native i64 support,
         but we handle them for completeness - treat as regular sub *)
      let dst_reg = Registers.find_exn registers dst in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      sub buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
    | Set { dst; value } ->
      let dst_reg = Registers.find_exn registers dst in
      if Registers.is_i64 registers dst
      then
        mov_imm_i64
          buf
          ~dst:dst_reg
          (Stdlib_upstream_compatible.Int64_u.of_int64 (Int64.of_int value))
      else mov_imm buf ~dst:dst_reg (I32.of_int value)
    | Mov { dst; src } ->
      let dst_reg = Registers.find_exn registers dst in
      let src_reg = Registers.find_exn registers src in
      if not (Register.equal dst_reg src_reg) then mov buf ~dst:dst_reg ~src:src_reg
    | C_call { dst; func; args } ->
      let dst_reg = Registers.find registers dst in
      let args_regs = List.map args ~f:(Registers.find_exn registers) in
      let clobbered_caller_saved_registers =
        (* FIXME: use a hashtable instead of a list. *)
        List.find_map
          registers.call_sites
          ~f:(fun (~block:block_idx, ~insn_idx:this_insn_idx, ~live_regs) ->
            if Mirl.Label.equal block_idx block.label && this_insn_idx = insn_idx
            then Some live_regs
            else None)
        |> Option.value_map ~default:[] ~f:Set.to_list
      in
      c_call buf ~dst:dst_reg ~func ~args:args_regs ~clobbered_caller_saved_registers
    | Return regs ->
      (match regs with
       | [ reg ] ->
         (* Single register return (typical for ARM64) *)
         let reg = Registers.find_exn registers reg in
         if not (Register.equal reg W0) then mov buf ~dst:W0 ~src:reg
       | _ -> failwith "ARM64 should only have single register returns");
      pop buf (Register.lr :: clobbered_callee_saved_registers);
      ret buf
    | Jump { target } -> b buf ~target:(label_name ~function_name ~label:target)
    | Branch { condition; target } ->
      let condition_reg = Registers.find_exn registers condition in
      tbnz buf ~condition:condition_reg ~target:(label_name ~function_name ~label:target))
;;

let emit_function (func : Mirl.Function.t) buf =
  let open Arm_dsl in
  let registers : Registers.t =
    let mapping, used, call_sites = Linscan.allocate_registers func in
    (* Create register_types table from function register descriptors *)
    let register_types = Virtual_register.Table.create () in
    Iarray.iteri func.registers ~f:(fun i { ty } ->
      let virt_reg = Virtual_register.of_int_exn i in
      Hashtbl.set register_types ~key:virt_reg ~data:ty);
    { mapping; used; call_sites; register_types }
  in
  let clobbered_callee_saved_registers =
    Set.inter registers.used Register.callee_saved |> Set.to_list
  in
  emit_function_prologue buf ~name:func.name;
  push buf (Register.lr :: clobbered_callee_saved_registers);
  let src, dst =
    List.filter_mapi func.params ~f:(fun i (_, src, _) ->
      let%bind.Option dst = Registers.find registers src in
      let src = Iarray.get Register.function_args i in
      Some (src, dst))
    |> List.unzip
  in
  iteri__local
    (Mucaml_backend_common.Parallel_move.parallel_move
       ~src:(Array.of_list src)
       ~dst:(Iarray.of_list dst)
       ~tmp:(fun _ -> W12))
    ~f:(fun (dst, src) -> mov buf ~dst ~src);
  Iarray.iter func.body ~f:(fun block ->
    emit_block
      block
      buf
      ~registers
      ~function_name:func.name
      ~clobbered_callee_saved_registers);
  emit_function_epilogue buf ~name:func.name
;;

let emit_mirl_without_prologue (program : Mirl.t) buf =
  List.iter program.functions ~f:(fun func -> emit_function func buf);
  Arm_dsl.to_string buf
;;

let emit_mirl (program : Mirl.t) =
  let open Arm_dsl in
  let buf = Arm_dsl.create () in
  emit_program_prologue buf;
  emit_mirl_without_prologue program buf
;;

module For_testing = struct
  let emit_mirl_without_prologue program =
    let buf = Arm_dsl.create () in
    emit_mirl_without_prologue program buf
  ;;
end
