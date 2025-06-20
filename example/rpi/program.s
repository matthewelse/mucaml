.syntax unified
.cpu cortex-m33
.thumb

.thumb_func
.type mucaml_main, %function
.globl mucaml_main
.fnstart
mucaml_main:
  mov r0, #10
  bx lr
.fnend
.size mucaml_main, . - mucaml_main

