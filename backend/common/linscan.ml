open! Core
open! Import
module Mirl = Mucaml_middle.Mirl
module Virtual_register = Mirl.Virtual_register

(* Linear scan register allocator.

   Algorithm described in detail in poletto and sarkar's paper:
   https://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf *)

module Make (Register : sig
    type t

    include Comparable.S_plain with type t := t

    val all_available_for_allocation : t list
    val caller_saved : Set.t
    val callee_saved : Set.t
  end) =
struct
  let live_intervals (func : Mirl.Function.t) =
    let live_intervals = Virtual_register.Table.create () in
    List.iter func.params ~f:(fun (_, reg, _) ->
      Hashtbl.set live_intervals ~key:reg ~data:(Some 0, None));
    let consumes ~idx reg =
      let idx = idx + 1 in
      Hashtbl.update live_intervals reg ~f:(function
        | None -> None, Some idx
        | Some (start, None) -> start, Some idx
        | Some (start, Some ends) -> start, Some (Int.max ends idx))
    in
    let assigns ~idx reg =
      let idx = idx + 1 in
      Hashtbl.update live_intervals reg ~f:(function
        | None -> Some idx, None
        | Some (Some start, ends) -> Some (Int.min start idx), ends
        | Some (None, ends) -> Some idx, ends)
    in
    let global_idx = stack_ (ref 0) in
    for block_idx = 0 to Iarray.length func.body - 1 do
      let block = Iarray.get func.body block_idx in
      (* Process each instruction in reverse order to build live intervals. *)
      for insn_idx = Iarray.length block.instructions - 1 downto 0 do
        let instruction = Iarray.get block.instructions insn_idx in
        let idx = insn_idx + !global_idx in
        match instruction with
        | Add { dst; src1; src2 } ->
          assigns ~idx dst;
          consumes ~idx src1;
          consumes ~idx src2
        | Sub { dst; src1; src2 } ->
          assigns ~idx dst;
          consumes ~idx src1;
          consumes ~idx src2
        | Set { dst; value = _ } -> assigns ~idx dst
        | C_call { dst; func = _; args } ->
          assigns ~idx dst;
          List.iter args ~f:(fun reg -> consumes ~idx reg)
        | Mov { dst; src } ->
          assigns ~idx dst;
          consumes ~idx src
        | Return reg -> consumes ~idx reg
        | Jump { target = _ } -> ()
        | Branch { condition; target = _ } -> consumes ~idx condition
      done;
      global_idx := !global_idx + Iarray.length block.instructions
    done;
    live_intervals
  ;;

  module Interval = struct
    type t =
      { start : int
      ; end_ : int
      }
    [@@deriving compare, sexp_of]

    module By_end = struct
      type nonrec t = t =
        { start : int
        ; end_ : int
        }
      [@@deriving compare, sexp_of]

      let flip { start; end_ } = { start = end_; end_ = start }
      let compare t t' = compare (flip t) (flip t')

      include functor Comparable.Make_plain
    end
  end

  (* Precondition [active] is non-empty. *)
  let spill_at_interval
    (active : Virtual_register.t Interval.By_end.Map.t)
    (current_start, current_end)
    current_register
    ~registers
    ~spills
    =
    let%tydi { start = spill_start; end_ = spill_end }, spill_virtual_register =
      Map.max_elt_exn active
    in
    let stolen_register = Hashtbl.find_exn registers spill_virtual_register in
    if spill_end > current_end
    then (
      Hashtbl.set registers ~key:current_register ~data:stolen_register;
      Queue.enqueue spills spill_virtual_register;
      let active = Map.remove active { start = spill_start; end_ = spill_end } in
      let active =
        Map.set
          active
          ~key:{ start = current_start; end_ = current_end }
          ~data:current_register
      in
      active)
    else (
      Queue.enqueue spills current_register;
      active)
  ;;

  let debug = false

  let expire_old_intervals active current_interval ~free_registers ~registers =
    let expired, active =
      Map.partitioni_tf active ~f:(fun ~key:interval ~data:_ ->
        if debug
        then
          print_s
            [%message
              "Checking interval" (interval : Interval.t) (current_interval : Interval.t)];
        current_interval.start >= interval.end_)
    in
    Map.iter expired ~f:(fun register ->
      if debug
      then print_endline [%string "Expiring register %{register#Virtual_register}"];
      free_registers := Set.add !free_registers (Hashtbl.find_exn registers register));
    active
  ;;

  let find_call_sites (func : Mirl.Function.t) =
    let call_sites = ref Int.Set.empty in
    let global_idx = ref 0 in
    for block_idx = 0 to Iarray.length func.body - 1 do
      let block = Iarray.get func.body block_idx in
      assert (Mirl.Label.to_int_exn block.label = block_idx);
      for insn_idx = 0 to Iarray.length block.instructions - 1 do
        let instruction = Iarray.get block.instructions insn_idx in
        (match instruction with
         | C_call { dst = _; func = _; args = _ } ->
           call_sites := Set.add !call_sites !global_idx
         | _ -> ());
        incr global_idx
      done
    done;
    !call_sites
  ;;

  (* Find call sites and compute which caller-saved registers need preservation *)
  let find_call_sites_with_live_registers (func : Mirl.Function.t) ~registers =
    let call_sites = ref [] in
    let global_idx = ref 0 in
    for block_idx = 0 to Iarray.length func.body - 1 do
      let block = Iarray.get func.body block_idx in
      assert (Mirl.Label.to_int_exn block.label = block_idx);
      for insn_idx = 0 to Iarray.length block.instructions - 1 do
        let instruction = Iarray.get block.instructions insn_idx in
        let idx = insn_idx + !global_idx in
        match instruction with
        | C_call { dst = _; func = _; args = _ } ->
          (* Find virtual registers that are live at this call site and allocated to caller-saved physical registers *)
          let live_caller_saved_registers =
            Hashtbl.fold
              registers
              ~init:Register.Set.empty
              ~f:(fun ~key:virtual_reg ~data:physical_reg acc ->
                if Set.mem Register.caller_saved physical_reg
                then (
                  (* Check if this virtual register is live at the call site *)
                  (* FIXME melse: It's not clear from the type of [live_intervals] how to
                     get the thing you actually want (live_out/live_in for which instruction). *)
                  let live_intervals = live_intervals func in
                  match Hashtbl.find live_intervals virtual_reg with
                  | Some (Some start, Some end_) when start <= idx && idx < end_ - 1 ->
                    Set.add acc physical_reg
                  | _ -> acc)
                else acc)
          in
          call_sites
          := (~block:block.label, ~insn_idx, ~live_regs:live_caller_saved_registers)
             :: !call_sites
        | _ -> ()
      done;
      global_idx := !global_idx + Iarray.length block.instructions
    done;
    List.rev !call_sites
  ;;

  let allocate_registers func =
    let free_registers =
      ref (Register.Set.of_list Register.all_available_for_allocation)
    in
    let live_intervals =
      live_intervals func
      |> Hashtbl.to_alist
      |> List.filter_map ~f:(fun (virtual_register, (live_start, live_end)) ->
        let%bind.Option live_start and live_end in
        Some ({ Interval.start = live_start; end_ = live_end }, virtual_register))
      |> List.sort ~compare:[%compare: Interval.t * Virtual_register.t]
    in
    let call_sites = find_call_sites func in
    let registers = Virtual_register.Table.create () in
    let _active =
      List.foldi
        live_intervals
        ~init:Interval.By_end.Map.empty
        ~f:(fun ix active (interval, virtual_register) ->
          let active = expire_old_intervals active interval ~free_registers ~registers in
          if debug
          then
            print_s
              [%message
                "Processing interval"
                  (ix : int)
                  (interval : Interval.t)
                  (virtual_register : Virtual_register.t)
                  (active : Virtual_register.t Interval.By_end.Map.t)
                  (!free_registers : Register.Set.t)];
          let reg =
            (* use a callee-saved register if this register is live across a call site *)
            let prefer_callee_saved_register =
              Set.exists call_sites ~f:(fun call_idx ->
                call_idx + 1 > interval.start && call_idx + 1 < interval.end_)
            in
            let registers_to_choose_from = !free_registers in
            let registers_to_choose_from =
              if prefer_callee_saved_register
              then (
                if debug
                then
                  print_s
                    [%message
                      "preferring callee saved registers"
                        (virtual_register : Virtual_register.t)
                        (call_sites : Int.Set.t)
                        (interval : Interval.t)];
                let available_callee_save_registers =
                  Set.inter registers_to_choose_from Register.callee_saved
                in
                if Set.is_empty available_callee_save_registers
                then registers_to_choose_from
                else available_callee_save_registers)
              else registers_to_choose_from
            in
            let available_register = Set.min_elt registers_to_choose_from in
            match available_register with
            | Some reg ->
              free_registers := Set.remove !free_registers reg;
              reg
            | None -> failwith "todo: spill"
          in
          Hashtbl.set registers ~key:virtual_register ~data:reg;
          Map.set active ~key:interval ~data:virtual_register)
    in
    let used_registers = Hashtbl.data registers |> Register.Set.of_list in
    let call_sites = find_call_sites_with_live_registers func ~registers in
    registers, used_registers, call_sites
  ;;
end

module%test _ = struct
  module Register = struct
    type t = int [@@deriving compare, sexp_of]

    include functor Comparable.Make_plain

    let all_available_for_allocation = List.init 8 ~f:Fn.id
    let caller_saved = Set.of_list [ 0; 1; 2; 3 ]
    let callee_saved = Set.of_list [ 4; 5; 6; 7 ]
    let to_string t = [%string "r%{t#Int}"]
  end

  module Allocator = Make (Register)
  open Allocator

  let%expect_test "live intervals" =
    let module Function = Mirl.Function in
    let module Block = Mirl.Block in
    let func =
      Function.build
        ~name:"test"
        ~params:[ "a", I32; "b", I32; "c", I32; "d", I32 ]
        (fun function_builder params ->
          let a, b, c, d =
            match params with
            | [ (_, a, _); (_, b, _); (_, c, _); (_, d, _) ] -> a, b, c, d
            | _ -> failwith "Expected 4 parameters"
          in
          Function.Builder.add_block' function_builder (fun block_builder ->
            let e = Function.Builder.fresh_register function_builder ~ty:I32 in
            let f = Function.Builder.fresh_register function_builder ~ty:I32 in
            let g = Function.Builder.fresh_register function_builder ~ty:I32 in
            let instructions : Mirl.Instruction.t list =
              [ Add { dst = e; src1 = d; src2 = a }
              ; Add { dst = f; src1 = b; src2 = c }
              ; Add { dst = f; src1 = f; src2 = b }
              ; Add { dst = d; src1 = e; src2 = f }
              ; Mov { dst = g; src = d }
              ; Return g
              ]
            in
            (* Create a block with the instructions. *)
            Block.Builder.push_many block_builder instructions)
          [@nontail])
    in
    let live_intervals = live_intervals func in
    let live_vars =
      Iarray.Local.create
        ~len:(Hashtbl.length live_intervals)
        { global = Virtual_register.Set.empty }
        ~mutate:(fun live_vars ->
          Hashtbl.iteri live_intervals ~f:(fun ~key:reg ~data:(start, ends) ->
            match start, ends with
            | None, None -> ()
            | Some _, None ->
              print_s [%message "Variable never used" (reg : Virtual_register.t)]
            | None, Some _ ->
              raise_s [%message "Variable never assigned" (reg : Virtual_register.t)]
            | Some start, Some end_ ->
              for idx = start to end_ - 1 do
                live_vars.(idx) <- { global = Set.add live_vars.(idx).global reg }
              done)
          [@nontail])
    in
    Iarray.Local.iter live_vars ~f:(fun { global = live } ->
      Set.to_list live
      |> List.map ~f:(fun reg ->
        let vars = [: "a"; "b"; "c"; "d"; "e"; "f"; "g" :] in
        Iarray.get vars (Virtual_register.to_int_exn reg))
      |> String.concat ~sep:", "
      |> print_endline);
    [%expect
      {|
    a, b, c, d
    b, c, d, e
    b, d, e, f
    d, e, f
    d
    g
    |}]
  ;;

  let%expect_test "slightly higher register pressure" =
    let module Function = Mirl.Function in
    let module Block = Mirl.Block in
    let func =
      Function.build
        ~name:"test"
        ~params:[ "x", I32 ]
        (fun function_builder params ->
          let x =
            match params with
            | [ (_, x, _) ] -> x
            | _ -> failwith "Expected 4 parameters"
          in
          let tmp1 = Function.Builder.fresh_register function_builder ~ty:I32 in
          let tmp2 = Function.Builder.fresh_register function_builder ~ty:I32 in
          let tmp3 = Function.Builder.fresh_register function_builder ~ty:I32 in
          let ret = Function.Builder.fresh_register function_builder ~ty:I32 in
          let y = Function.Builder.fresh_register function_builder ~ty:I32 in
          let z = Function.Builder.fresh_register function_builder ~ty:I32 in
          let a = Function.Builder.fresh_register function_builder ~ty:I32 in
          Function.Builder.add_block' function_builder (fun block ->
            let instructions : Mirl.Instruction.t list =
              [ Set { dst = tmp1; value = 100 }
              ; C_call { func = "f"; dst = y; args = [ tmp1 ] }
              ; C_call { func = "f"; dst = z; args = [ x ] }
              ; C_call { func = "f"; dst = a; args = [ y ] }
              ; Add { dst = tmp2; src1 = a; src2 = z }
              ; Add { dst = tmp3; src1 = tmp2; src2 = y }
              ; Add { dst = ret; src1 = tmp3; src2 = x }
              ; Return ret
              ]
            in
            Block.Builder.push_many block instructions))
    in
    let live_intervals = live_intervals func in
    let live_vars =
      Iarray.Local.create
        ~len:(Hashtbl.length live_intervals)
        { global = Virtual_register.Set.empty }
        ~mutate:(fun live_vars ->
          Hashtbl.iteri live_intervals ~f:(fun ~key:reg ~data:(start, ends) ->
            match start, ends with
            | None, None -> ()
            | Some _, None ->
              print_s [%message "Variable never used" (reg : Virtual_register.t)]
            | None, Some _ ->
              raise_s [%message "Variable never assigned" (reg : Virtual_register.t)]
            | Some start, Some end_ ->
              for idx = start to end_ - 1 do
                live_vars.(idx) <- { global = Set.add live_vars.(idx).global reg }
              done)
          [@nontail])
    in
    Iarray.Local.iter live_vars ~f:(fun { global = live } ->
      Set.to_list live
      |> List.map ~f:(fun reg ->
        let vars = [: "x"; "t0"; "t1"; "t2"; "r"; "y"; "x"; "a" :] in
        Iarray.get vars (Virtual_register.to_int_exn reg))
      |> String.concat ~sep:", "
      |> print_endline);
    [%expect
      {|
      x
      x, t0
      x, y
      x, y, x
      x, y, x, a
      x, t1, y
      x, t2
      r
      |}]
  ;;

  let print_program (func : Mirl.Function.t) ~registers =
    Iarray.iter func.body ~f:(fun bloxk ->
      let instructions = bloxk.instructions in
      print_endline [%string "%{bloxk.label#Mirl.Label}:"];
      Iarray.iter instructions ~f:(fun instruction ->
        match instruction with
        | Add { dst; src1; src2 } ->
          let dst_reg = Hashtbl.find_exn registers dst in
          let src1_reg = Hashtbl.find_exn registers src1 in
          let src2_reg = Hashtbl.find_exn registers src2 in
          print_endline
            [%string "%{dst_reg#Register} := %{src1_reg#Register} + %{src2_reg#Register}"]
        | Sub { dst; src1; src2 } ->
          let dst_reg = Hashtbl.find_exn registers dst in
          let src1_reg = Hashtbl.find_exn registers src1 in
          let src2_reg = Hashtbl.find_exn registers src2 in
          print_endline
            [%string "%{dst_reg#Register} := %{src1_reg#Register} - %{src2_reg#Register}"]
        | Set { dst; value = _ } ->
          let dst_reg = Hashtbl.find_exn registers dst in
          print_endline [%string "%{dst_reg#Register} := <set value>"]
        | C_call { dst; func = _; args } ->
          let dst_reg = Hashtbl.find_exn registers dst in
          let args_regs =
            List.map args ~f:(Hashtbl.find_exn registers)
            |> List.map ~f:Register.to_string
            |> String.concat ~sep:", "
          in
          print_endline [%string "%{dst_reg#Register} := c_call(%{args_regs})"]
        | Mov { dst; src } ->
          let dst_reg = Hashtbl.find_exn registers dst in
          let src_reg = Hashtbl.find_exn registers src in
          print_endline [%string "%{dst_reg#Register} := %{src_reg#Register}"]
        | Return reg ->
          let reg = Hashtbl.find_exn registers reg in
          print_endline [%string "return %{reg#Register}"]
        | Jump { target } -> print_endline [%string "jump %{target#Mirl.Label}"]
        | Branch { condition; target } ->
          let condition_reg = Hashtbl.find_exn registers condition in
          print_endline
            [%string "branch if %{condition_reg#Register} to %{target#Mirl.Label}"]))
  ;;

  let%expect_test "allocate registers" =
    let module Function = Mirl.Function in
    let module Block = Mirl.Block in
    let func =
      Function.build
        ~name:"test"
        ~params:[ "a", I32; "b", I32; "c", I32; "d", I32 ]
        (fun function_builder params ->
          let a, b, c, d =
            match params with
            | [ (_, a, _); (_, b, _); (_, c, _); (_, d, _) ] -> a, b, c, d
            | _ -> failwith "Expected 4 parameters"
          in
          Function.Builder.add_block' function_builder (fun block_builder ->
            let e = Function.Builder.fresh_register function_builder ~ty:I32 in
            let f = Function.Builder.fresh_register function_builder ~ty:I32 in
            let g = Function.Builder.fresh_register function_builder ~ty:I32 in
            let instructions : Mirl.Instruction.t list =
              [ Add { dst = e; src1 = d; src2 = a }
              ; Add { dst = f; src1 = b; src2 = c }
              ; Add { dst = f; src1 = f; src2 = b }
              ; Add { dst = d; src1 = e; src2 = f }
              ; Mov { dst = g; src = d }
              ; Return g
              ]
            in
            (* Create a block with the instructions. *)
            Block.Builder.push_many block_builder instructions)
          [@nontail])
    in
    let registers, _, _ = allocate_registers func in
    print_program func ~registers;
    [%expect
      {|
      block_0:
      r0 := r3 + r0
      r1 := r2 + r1
      r1 := r1 + r2
      r3 := r0 + r1
      r0 := r3
      return r0
      |}]
  ;;

  let%expect_test "branchy eample" =
    let module Function = Mirl.Function in
    let module Block = Mirl.Block in
    let func =
      Function.build ~name:"test" ~params:[] (fun function_builder _ ->
        let x = Function.Builder.fresh_register function_builder ~ty:I32 in
        let y = Function.Builder.fresh_register function_builder ~ty:I32 in
        let block_1 = Function.Builder.add_block function_builder ignore in
        let block_2 = Function.Builder.add_block function_builder ignore in
        let block_3 = Function.Builder.add_block function_builder ignore in
        let instructions : Mirl.Instruction.t list =
          [ Set { dst = x; value = 2 }; Jump { target = Block.Builder.label block_3 } ]
        in
        Block.Builder.push_many block_1 instructions;
        let instructions : Mirl.Instruction.t list =
          [ Set { dst = x; value = 2 }; Jump { target = Block.Builder.label block_2 } ]
        in
        Block.Builder.push_many block_2 instructions;
        let instructions : Mirl.Instruction.t list =
          [ Add { dst = y; src1 = x; src2 = x }; Return y ]
        in
        Block.Builder.push_many block_3 instructions;
        ())
    in
    let registers, _, _ = allocate_registers func in
    print_program func ~registers;
    [%expect
      {|
      block_0:
      r0 := <set value>
      jump block_2
      block_1:
      r0 := <set value>
      jump block_1
      block_2:
      r0 := r0 + r0
      return r0
      |}]
  ;;

  let%expect_test "slightly higher register pressure" =
    let module Function = Mirl.Function in
    let module Block = Mirl.Block in
    let func =
      Function.build
        ~name:"test"
        ~params:[ "x", I32 ]
        (fun function_builder params ->
          let x =
            match params with
            | [ (_, x, _) ] -> x
            | _ -> failwith "Expected 4 parameters"
          in
          let tmp1 = Function.Builder.fresh_register function_builder ~ty:I32 in
          let tmp2 = Function.Builder.fresh_register function_builder ~ty:I32 in
          let tmp3 = Function.Builder.fresh_register function_builder ~ty:I32 in
          let ret = Function.Builder.fresh_register function_builder ~ty:I32 in
          let y = Function.Builder.fresh_register function_builder ~ty:I32 in
          let z = Function.Builder.fresh_register function_builder ~ty:I32 in
          let a = Function.Builder.fresh_register function_builder ~ty:I32 in
          Function.Builder.add_block' function_builder (fun block ->
            let instructions : Mirl.Instruction.t list =
              [ Set { dst = tmp1; value = 100 }
              ; C_call { func = "f"; dst = y; args = [ tmp1 ] }
              ; C_call { func = "f"; dst = z; args = [ x ] }
              ; C_call { func = "f"; dst = a; args = [ y ] }
              ; Add { dst = tmp2; src1 = a; src2 = z }
              ; Add { dst = tmp3; src1 = tmp2; src2 = y }
              ; Add { dst = ret; src1 = tmp3; src2 = x }
              ; Return ret
              ]
            in
            Block.Builder.push_many block instructions))
    in
    let registers, _, _ = allocate_registers func in
    print_program func ~registers;
    [%expect
      {|
      block_0:
      r0 := <set value>
      r5 := c_call(r0)
      r6 := c_call(r4)
      r0 := c_call(r5)
      r0 := r0 + r6
      r0 := r0 + r5
      r0 := r0 + r4
      return r0
      |}]
  ;;
end
