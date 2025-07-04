  $ mu build -d assembly
  .type mucaml_main, %function
  .globl mucaml_main
  mucaml_main:
    stp lr, x19, [sp, #-16]!
    mov w19, w0
  mucaml_main__block_0:
    mov w0, #5
    add w0, w19, w0
    bl mucaml_print
    add w0, w0, w19
    ldp lr, x19, [sp], #16
    ret
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  5
  error: Error: ("Process.run failed" (prog ./test_call.elf) (args ()) (working_dir ())
   (env (Extend ())) (exit_status (Exit_non_zero 5)))
  [1]
