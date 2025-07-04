  $ mu build -d assembly
  .type mucaml_main, %function
  .globl mucaml_main
  mucaml_main:
    str lr, [sp, #-16]!
  mucaml_main__block_0:
    mov w0, #1000
    ldr lr, [sp], #16
    ret
  .size mucaml_main, . - mucaml_main
  
  $ mu run
  error: Error: ("Process.run failed" (prog ./hello_world.elf) (args ()) (working_dir ())
   (env (Extend ())) (exit_status (Exit_non_zero 232)))
  [1]
