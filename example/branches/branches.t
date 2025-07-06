  $ mu build -d assembly
  .type mucaml_main, %function
  .globl mucaml_main
  mucaml_main:
    str lr, [sp, #-16]!
  mucaml_main__block_0:
    tbnz w0, #0, mucaml_main__block_1
    b mucaml_main__block_2
  mucaml_main__block_1:
    mov w0, #3
    mov w1, #4
    add w0, w0, w1
    b mucaml_main__block_3
  mucaml_main__block_2:
    mov w1, #5
    mov w2, #6
    add w1, w1, w2
    mov w0, w1
    b mucaml_main__block_3
  mucaml_main__block_3:
    ldr lr, [sp], #16
    ret
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  [11]
