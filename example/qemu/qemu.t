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
    cbnz r0, mucaml_main__block_1
    b mucaml_main__block_2
  mucaml_main__block_1:
    mov r0, #10
    bl mucaml_print
    mov r0, #100
    b mucaml_main__block_3
  mucaml_main__block_2:
    mov r1, #99
    mov r0, r1
    b mucaml_main__block_3
  mucaml_main__block_3:
    bl mucaml_exit
    pop {pc}
  .fnend
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  mucaml_exit  99
  error: Error: ("Process.run failed" (prog qemu-system-arm)
   (args
    (-machine stm32vldiscovery -cpu cortex-m3 -nographic -semihosting-config
     enable=on,target=native -m 16M -kernel ./blinky.elf))
   (working_dir ()) (env (Extend ())) (exit_status (Exit_non_zero 99)))
  [1]
