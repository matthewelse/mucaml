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

let emit_newline t = add_char t '\n'

let emit_program_prologue t =
  emit_line t ".syntax unified";
  emit_line t [%string ".cpu cortex-m33"];
  emit_line t ".thumb";
  emit_newline t
;;

let emit_function_prologue t ~name =
  emit_line t ".thumb_func";
  emit_line t [%string ".type %{name}, %function"];
  emit_line t [%string ".globl %{name}"];
  emit_line t ".fnstart";
  emit_line t [%string "%{name}:"]
;;

let emit_function_epilogue t ~name =
  emit_line t ".fnend";
  emit_line t [%string ".size %{name}, . - %{name}"]
;;

let pop t regs =
  let regs = List.map regs ~f:Register.to_string |> String.concat ~sep:"," in
  emit_line t [%string "  pop {%{regs}}"]
;;

let push t regs =
  let regs = List.map regs ~f:Register.to_string |> String.concat ~sep:"," in
  emit_line t [%string "  push {%{regs}}"]
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

let ret t = emit_line t "  bx lr"

let add t ~dst ~src1 ~src2 =
  emit_line t [%string "  add %{dst#Register}, %{src1#Register}, %{src2#Register}"]
;;

let sub t ~dst ~src1 ~src2 =
  emit_line t [%string "  sub %{dst#Register}, %{src1#Register}, %{src2#Register}"]
;;

let b t ~target = emit_line t [%string "  b %{target}"]
let bl t ~func = emit_line t [%string "  bl %{func}"]
let label t label = emit_line t [%string "%{label}:"]

let cbnz t ~condition ~target =
  emit_line t [%string "  cbnz %{condition#Register}, %{target}"]
;;
