open! Core
open! Import

module Status = struct
  type t =
    | To_move
    | Being_moved
    | Moved
end

let ( .:() ) = Iarray.( .:() )

let parallel_move ~(src : 'reg array @ local) ~(dst : 'reg iarray @ local) ~tmp =
  (* Credit to https://compiler.club/parallel-moves/ and Leroy, et al.
     https://xavierleroy.org/publi/parallel-move.pdf *)
  assert (Array.length src = Iarray.length dst);
  let length = Array.length src in
  exclave_
  let status = (Array.create [@alloc stack]) ~len:length Status.To_move in
  let rec move_one i ~acc = exclave_
    if not (phys_equal src.(i) dst.:(i))
    then (
      status.(i) <- Being_moved;
      let rec loop j ~acc = exclave_
        if j < length
        then (
          let acc =
            if phys_equal src.(j) dst.:(i)
            then (
              match status.(j) with
              | To_move -> move_one j ~acc
              | Being_moved ->
                let t = tmp src.(j) in
                let src_reg = src.(j) in
                src.(j) <- t;
                (t, src_reg) :: acc
              | Moved -> acc)
            else acc
          in
          loop (j + 1) ~acc [@nontail])
        else acc
      in
      let acc = loop 0 ~acc in
      let acc = stack_ ((dst.:(i), src.(i)) :: acc) in
      status.(i) <- Moved;
      acc)
    else acc
  in
  let rec iter i ~acc = exclave_
    if i >= length
    then acc
    else (
      let acc =
        match status.(i) with
        | To_move -> move_one i ~acc
        | _ -> acc
      in
      iter (i + 1) ~acc)
  in
  let revd = iter 0 ~acc:[] in
  (List.rev [@alloc stack]) revd [@nontail]
;;

let%expect_test "test [parallel_move]" =
  let tmp _ = "tmp" in
  let rdi, rsi, rdx, rcx, r8, r9 = "rdi", "rsi", "rdx", "rcx", "r8", "r9" in
  let dst = [: rdi; rsi; rdx; rcx; r8; r9 :] in
  let src = [| rsi; rdi; rsi; rsi; r9; r8 |] in
  let moves = parallel_move ~src ~dst ~tmp in
  let rec aux moves =
    match moves with
    | [] -> ()
    | (dst, src) :: tl ->
      print_endline [%string "%{dst} := %{src}"];
      aux tl
  in
  aux moves;
  [%expect
    {|
    tmp := rsi
    rdx := rsi
    rcx := rsi
    rsi := rdi
    rdi := tmp
    tmp := r9
    r9 := r8
    r8 := tmp
    |}]
;;
