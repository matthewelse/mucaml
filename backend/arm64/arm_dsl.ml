open! Core
open! Import
open Bigbuffer

type t = Bigbuffer.t

let create () = Bigbuffer.create 1024
let to_string t = Bigbuffer.contents t

let emit_line t line =
  add_string t line;
  add_char t '\n'
;;

let emit_program_prologue _ = ()

let emit_function_prologue t ~name =
  emit_line t [%string ".type %{name}, %function"];
  emit_line t [%string ".globl %{name}"];
  emit_line t [%string "%{name}:"]
;;

let emit_function_epilogue t ~name = emit_line t [%string ".size %{name}, . - %{name}"]

let pop t regs =
  (* AArch64 stack pop: load from stack and increment SP, maintaining 16-byte alignment *)
  (* Pop in reverse order of push to restore correct values *)
  let rec pop_pairs acc = function
    | [] -> acc
    | [ reg ] -> (reg, None) :: acc
    | reg1 :: reg2 :: rest -> pop_pairs ((reg1, Some reg2) :: acc) rest
  in
  let pairs = pop_pairs [] regs in
  List.iter pairs ~f:(function
    | reg, None ->
      (* Single register - use 16-byte aligned access to maintain alignment *)
      emit_line t [%string "  ldr %{reg#Register.Sixty_four}, [sp], #16"]
    | reg1, Some reg2 ->
      emit_line
        t
        [%string
          "  ldp %{reg1#Register.Sixty_four}, %{reg2#Register.Sixty_four}, [sp], #16"])
;;

let push t regs =
  (* AArch64 stack push: decrement SP and store, maintaining 16-byte alignment *)
  let rec make_pairs acc = function
    | [] -> List.rev acc
    | [ reg ] -> List.rev ((reg, None) :: acc)
    | reg1 :: reg2 :: rest -> make_pairs ((reg1, Some reg2) :: acc) rest
  in
  let pairs = make_pairs [] regs in
  List.iter pairs ~f:(function
    | reg, None ->
      (* Single register - use 16-byte aligned access to maintain alignment *)
      emit_line t [%string "  str %{reg#Register.Sixty_four}, [sp, #-16]!"]
    | reg1, Some reg2 ->
      emit_line
        t
        [%string
          "  stp %{reg1#Register.Sixty_four}, %{reg2#Register.Sixty_four}, [sp, #-16]!"])
;;

let mov t ~dst ~src = emit_line t [%string "  mov %{dst#Register}, %{src#Register}"]

let is_valid_arm_imm value =
  let open I32.O in
  (* ARM immediate values must be in the range 0-255 and must be a multiple of 4 *)
  let tz = I32.ctz value in
  let tz = Int.O.(tz land lnot 1) in
  let value = value lsr tz in
  I32.unsigned_compare value #256l < 0
;;

let mov_imm t ~dst value =
  if is_valid_arm_imm value
  then emit_line t [%string "  mov %{dst#Register}, #%{value#I32}"]
  else (
    let lw = I32.O.(value land #0xFFFFl) in
    let hw = I32.O.(value lsr 16) in
    emit_line t [%string "  mov %{dst#Register}, #%{lw#I32}"];
    emit_line t [%string "  movt %{dst#Register}, #%{hw#I32}"])
;;

let mov_imm_i64 t ~dst (value : I64.t) =
  let open I64.O in
  (* For ARM64, we can use MOV with immediate for small values, or build larger values
     using MOVZ/MOVK sequence *)
  if value >= #0L && value <= #65535L
  then emit_line t [%string "  mov %{dst#Register.Sixty_four}, #%{value#I64}"]
  else (
    (* Use MOVZ/MOVK sequence for larger values *)
    let w0 = value land #0xFFFFL in
    let w1 = (value lsr 16) land #0xFFFFL in
    let w2 = (value lsr 32) land #0xFFFFL in
    let w3 = (value lsr 48) land #0xFFFFL in
    emit_line t [%string "  movz %{dst#Register.Sixty_four}, #%{w0#I64}"];
    if not (w1 = #0L)
    then emit_line t [%string "  movk %{dst#Register.Sixty_four}, #%{w1#I64}, lsl #16"];
    if not (w2 = #0L)
    then emit_line t [%string "  movk %{dst#Register.Sixty_four}, #%{w2#I64}, lsl #32"];
    if not (w3 = #0L)
    then emit_line t [%string "  movk %{dst#Register.Sixty_four}, #%{w3#I64}, lsl #48"])
;;

let ret t = emit_line t "  ret"

let add t ~dst ~src1 ~src2 =
  emit_line t [%string "  add %{dst#Register}, %{src1#Register}, %{src2#Register}"]
;;

let sub t ~dst ~src1 ~src2 =
  emit_line t [%string "  sub %{dst#Register}, %{src1#Register}, %{src2#Register}"]
;;

let add_i64 t ~dst ~src1 ~src2 =
  emit_line
    t
    [%string
      "  add %{dst#Register.Sixty_four}, %{src1#Register.Sixty_four}, \
       %{src2#Register.Sixty_four}"]
;;

let sub_i64 t ~dst ~src1 ~src2 =
  emit_line
    t
    [%string
      "  sub %{dst#Register.Sixty_four}, %{src1#Register.Sixty_four}, \
       %{src2#Register.Sixty_four}"]
;;

let bl t ~func = emit_line t [%string "  bl %{func}"]
let b t ~target = emit_line t [%string "  b %{target}"]
let label t label = emit_line t [%string "%{label}:"]

let tbnz t ~condition ~target =
  emit_line t [%string "  tbnz %{condition#Register}, #0, %{target}"]
;;
