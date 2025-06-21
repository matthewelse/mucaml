open! Core
module Cmm = Mucaml_middle.Cmm

let live_intervals (block : Cmm.Block.t) ~inputs =
  let live_intervals = Cmm.Register.Table.create () in
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

let%expect_test "live_intervals" =
  Cmm.Register.For_testing.reset_counter ();
  let a = Cmm.Register.create () in
  let b = Cmm.Register.create () in
  let c = Cmm.Register.create () in
  let d = Cmm.Register.create () in
  let e = Cmm.Register.create () in
  let f = Cmm.Register.create () in
  let g = Cmm.Register.create () in
  let (instructions, g) : Cmm.Instruction.t iarray * Cmm.Register.t =
    ( [: Add { dest = e; src1 = d; src2 = a }
       ; Add { dest = f; src1 = b; src2 = c }
       ; Add { dest = f; src1 = f; src2 = b }
       ; Add { dest = d; src1 = e; src2 = f }
       ; Mov { dest = g; src = d }
      :]
    , g )
  in
  let block : Cmm.Block.t = { instructions; terminator = Return g } in
  let live_intervals = live_intervals block ~inputs:[ a; b; c; d ] in
  let live_vars =
    Iarray.Local.create
      ~len:(Iarray.length block.instructions + 1)
      { global = Cmm.Register.Set.empty }
      ~mutate:(fun live_vars ->
        Hashtbl.iteri live_intervals ~f:(fun ~key:reg ~data:(start, ends) ->
          match start, ends with
          | None, None -> ()
          | Some _, None ->
            print_s [%message "Variable never used" (reg : Cmm.Register.t)]
          | None, Some _ ->
            raise_s [%message "Variable never assigned" (reg : Cmm.Register.t)]
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
      Iarray.get vars (Cmm.Register.to_int_exn reg))
    |> String.concat ~sep:", "
    |> print_endline);
  [%expect {|
    a, b, c, d
    b, c, d, e
    b, d, e, f
    d, e, f
    d
    g
    |}]
;;
