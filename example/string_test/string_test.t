  $ mu build -d assembly
  .section .rodata, "a", @progbits
  .globl global_str_0
  global_str_0:
  .asciz "Hello, World!
  "
  .size global_str_0, . - global_str_0
  
  .section .text
  .type mucaml_main, %function
  .globl mucaml_main
  mucaml_main:
    str lr, [sp, #-16]!
  mucaml_main__block_0:
    ldr x0, =global_str_0
    bl puts
    mov w0, #0
    ldr lr, [sp], #16
    ret
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  Hello, World!
  
