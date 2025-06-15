open! Core
open Bigbuffer

type t = Bigbuffer.t

let create () = Bigbuffer.create 1024
let to_string t = Bigbuffer.contents t

let emit_line t line =
  add_string t line;
  add_char t '\n'
;;

let emit_newline t = add_char t '\n'

let emit_program_prologue t ~target =
  emit_line t ".syntax unified";
  emit_line t [%string ".cpu %{target#Target}"];
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
let mov_imm t ~dst value = emit_line t [%string "  mov %{dst#Register}, #%{value#Int}"]
let ret t = emit_line t "  bx lr"
