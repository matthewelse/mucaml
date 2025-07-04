  $ mu build -d assembly
  .syntax unified
  .cpu cortex-m33
  .thumb
  
  .thumb_func
  .type mucaml_main, %function
  .globl mucaml_main
  .fnstart
  mucaml_main:
    push {lr}
  mucaml_main__block_0:
    cbnz r0, mucaml_main__block_2
    b mucaml_main__block_3
  mucaml_main__block_1:
    bl mucaml_exit
    pop {pc}
  mucaml_main__block_2:
    mov r0, #10
    bl mucaml_print
    mov r0, #100
    b mucaml_main__block_1
  mucaml_main__block_3:
    mov r0, #99
    b mucaml_main__block_1
  .fnend
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  mucaml_exit: 99
