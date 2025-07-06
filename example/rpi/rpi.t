Raspberry Pi Zero 2 example. We only build the assembly, since there are external
dependencies for assembling and linking.

  $ mu build -d assembly -stop-after assembly
  .syntax unified
  .cpu cortex-m33
  .thumb
  
  .thumb_func
  .type mucaml_main, %function
  .globl mucaml_main
  .fnstart
  mucaml_main:
    push {r4,lr}
  mucaml_main__block_0:
    mov r0, #0
    bl mucaml_pin
    mov r4, r0
    mov r0, r4
    bl mucaml_pin_toggle
    mov r0, #1000
    bl mucaml_sleep
    mov r0, r4
    bl mucaml_pin_toggle
    mov r0, #1000
    bl mucaml_sleep
    mov r0, r4
    bl mucaml_pin_toggle
    mov r0, #1000
    bl mucaml_sleep
    mov r0, r4
    bl mucaml_pin_toggle
    mov r0, #1000
    bl mucaml_sleep
    mov r0, r4
    bl mucaml_pin_toggle
    mov r0, #1000
    bl mucaml_sleep
    mov r0, r4
    bl mucaml_pin_toggle
    mov r0, #1000
    bl mucaml_sleep
    mov r0, #0
    pop {r4,pc}
  .fnend
  .size mucaml_main, . - mucaml_main
  
