open! Core
module Mirl = Mucaml_middle.Mirl
module Virtual_register = Mirl.Virtual_register

(* Linear scan register allocator.

   Algorithm described in detail in poletto and sarkar's paper:
   https://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf *)

module Make (Register : sig
    type t

    include Comparable.S_plain with type t := t

    val all_available_for_allocation : t list
  end) =
struct
  let live_intervals (block : Mirl.Block.t) ~inputs =
    let live_intervals = Virtual_register.Table.create () in
    List.iter inputs ~f:(fun reg ->
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
    for idx = Iarray.length block.instructions - 1 downto 0 do
      let instruction = Iarray.get block.instructions idx in
      match instruction with
      | Add { dest; src1; src2 } ->
        assigns ~idx dest;
        consumes ~idx src1;
        consumes ~idx src2
      | Sub { dest; src1; src2 } ->
        assigns ~idx dest;
        consumes ~idx src1;
        consumes ~idx src2
      | Set { dest; value = _ } -> assigns ~idx dest
      | C_call { dest; func = _; args } ->
        assigns ~idx dest;
        List.iter args ~f:(fun reg -> consumes ~idx reg)
      | Mov { dest; src } ->
        assigns ~idx dest;
        consumes ~idx src
    done;
    (match block.terminator with
     | Return reg -> consumes ~idx:(Iarray.length block.instructions) reg);
    live_intervals
  ;;

  module Interval = struct
    type t = int * int [@@deriving compare, sexp_of]

    module By_end = struct
      type t = int * int [@@deriving compare, sexp_of]

      let compare (start, end_) = compare (end_, start)

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
    let (spill_start, spill_end), spill_virtual_register = Map.max_elt_exn active in
    let stolen_register = Hashtbl.find_exn registers spill_virtual_register in
    if spill_end > current_end
    then (
      Hashtbl.set registers ~key:current_register ~data:stolen_register;
      Queue.enqueue spills spill_virtual_register;
      let active = Map.remove active (spill_start, spill_end) in
      let active =
        Map.set active ~key:(current_start, current_end) ~data:current_register
      in
      active)
    else (
      Queue.enqueue spills current_register;
      active)
  ;;

  let expire_old_intervals active current_interval ~free_registers ~registers =
    let expired, active = Map.split_lt_ge active current_interval in
    Map.iter expired ~f:(fun register ->
      Queue.enqueue free_registers (Hashtbl.find_exn registers register));
    active
  ;;

  let allocate_registers block ~inputs =
    let free_registers = Queue.of_list Register.all_available_for_allocation in
    let live_intervals =
      live_intervals block ~inputs
      |> Hashtbl.to_alist
      |> List.filter_map ~f:(fun (virtual_register, (live_start, live_end)) ->
        let%bind.Option live_start and live_end in
        Some ((live_start, live_end), virtual_register))
      |> List.sort ~compare:[%compare: Interval.t * Virtual_register.t]
    in
    let registers = Virtual_register.Table.create () in
    let _active =
      List.fold
        live_intervals
        ~init:Interval.By_end.Map.empty
        ~f:(fun active (interval, virtual_register) ->
          let active = expire_old_intervals active interval ~free_registers ~registers in
          let reg =
            match Queue.dequeue free_registers with
            | Some reg -> reg
            | None -> failwith "todo: spill"
          in
          Hashtbl.set registers ~key:virtual_register ~data:reg;
          Map.set active ~key:interval ~data:virtual_register)
    in
    let used_registers = Hashtbl.data registers |> Register.Set.of_list in
    registers, used_registers
  ;;
end

module%test _ = struct
  module Register = struct
    type t = int [@@deriving compare, sexp_of]

    include functor Comparable.Make_plain

    let all_available_for_allocation = List.init 8 ~f:Fn.id
    let to_string t = [%string "r%{t#Int}"]
  end

  module Allocator = Make (Register)
  open Allocator

  let%expect_test "live_intervals" =
    Virtual_register.For_testing.reset_counter ();
    let a = Virtual_register.create () in
    let b = Virtual_register.create () in
    let c = Virtual_register.create () in
    let d = Virtual_register.create () in
    let e = Virtual_register.create () in
    let f = Virtual_register.create () in
    let g = Virtual_register.create () in
    let (instructions, g) : Mirl.Instruction.t iarray * Virtual_register.t =
      ( [: Add { dest = e; src1 = d; src2 = a }
         ; Add { dest = f; src1 = b; src2 = c }
         ; Add { dest = f; src1 = f; src2 = b }
         ; Add { dest = d; src1 = e; src2 = f }
         ; Mov { dest = g; src = d }
        :]
      , g )
    in
    let block : Mirl.Block.t = { instructions; terminator = Return g } in
    let live_intervals = live_intervals block ~inputs:[ a; b; c; d ] in
    let live_vars =
      Iarray.Local.create
        ~len:(Iarray.length block.instructions + 1)
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

  let%expect_test "allocate_registers" =
    Virtual_register.For_testing.reset_counter ();
    let a = Virtual_register.create () in
    let b = Virtual_register.create () in
    let c = Virtual_register.create () in
    let d = Virtual_register.create () in
    let e = Virtual_register.create () in
    let f = Virtual_register.create () in
    let g = Virtual_register.create () in
    let (instructions, g) : Mirl.Instruction.t iarray * Virtual_register.t =
      ( [: Add { dest = e; src1 = d; src2 = a }
         ; Add { dest = f; src1 = b; src2 = c }
         ; Add { dest = f; src1 = f; src2 = b }
         ; Add { dest = d; src1 = e; src2 = f }
         ; Mov { dest = g; src = d }
        :]
      , g )
    in
    let block : Mirl.Block.t = { instructions; terminator = Return g } in
    let inputs = [ a; b; c; d ] in
    let registers, _ = allocate_registers block ~inputs in
    Iarray.iter instructions ~f:(fun instruction ->
      match instruction with
      | Add { dest; src1; src2 } ->
        let dest_reg = Hashtbl.find_exn registers dest in
        let src1_reg = Hashtbl.find_exn registers src1 in
        let src2_reg = Hashtbl.find_exn registers src2 in
        print_endline
          [%string "%{dest_reg#Register} := %{src1_reg#Register} + %{src2_reg#Register}"]
      | Sub { dest; src1; src2 } ->
        let dest_reg = Hashtbl.find_exn registers dest in
        let src1_reg = Hashtbl.find_exn registers src1 in
        let src2_reg = Hashtbl.find_exn registers src2 in
        print_endline
          [%string "%{dest_reg#Register} := %{src1_reg#Register} - %{src2_reg#Register}"]
      | Set { dest; value = _ } ->
        let dest_reg = Hashtbl.find_exn registers dest in
        print_endline [%string "%{dest_reg#Register} := <set value>"]
      | C_call { dest; func = _; args } ->
        let dest_reg = Hashtbl.find_exn registers dest in
        let args_regs =
          List.map args ~f:(Hashtbl.find_exn registers)
          |> List.map ~f:Register.to_string
          |> String.concat ~sep:", "
        in
        print_endline [%string "%{dest_reg#Register} := c_call(%{args_regs})"]
      | Mov { dest; src } ->
        let dest_reg = Hashtbl.find_exn registers dest in
        let src_reg = Hashtbl.find_exn registers src in
        print_endline [%string "%{dest_reg#Register} := %{src_reg#Register}"]);
    [%expect
      {|
    r4 := r3 + r0
    r5 := r2 + r1
    r5 := r5 + r2
    r3 := r4 + r5
    r6 := r3
    |}]
  ;;
end
