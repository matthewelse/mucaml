  $ mu build -d assembly
  .type mucaml_main, %function
  .globl mucaml_main
  mucaml_main:
    str lr, [sp, #-16]!
  mucaml_main__block_0:
    tbnz w0, #0, mucaml_main__block_2
    b mucaml_main__block_3
  mucaml_main__block_1:
    ldr lr, [sp], #16
    ret
  mucaml_main__block_2:
    mov w0, #3
    mov w1, #4
    add w0, w0, w1
    b mucaml_main__block_1
  mucaml_main__block_3:
    mov w0, #5
    mov w1, #6
    add w0, w0, w1
    b mucaml_main__block_1
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  error: Error: ("Process.run failed" (prog ./branches.elf) (args ()) (working_dir ())
   (env (Extend ())) (exit_status (Exit_non_zero 11)))
  [1]
