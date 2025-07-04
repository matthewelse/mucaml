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
    push {lr}
  mucaml_main__block_0:
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #1000
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_on
    mov r0, #100
    bl mucaml_sleep_ms
    mov r0, #7
    bl mucaml_led_off
    mov r0, #1000
    pop {pc}
  .fnend
  .size mucaml_main, . - mucaml_main
  
