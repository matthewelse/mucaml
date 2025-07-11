
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"


@global_const_0 = private unnamed_addr constant [14 x i8] c"Hello, World!\00"

declare i32 @mucaml_print_endline(ptr)

define i32 @mucaml_main(i32 %r0) {
  %r1 = getelementptr [14 x i8], ptr @global_const_0, i64 0, i64 0
  %r2 = call i32 @mucaml_print_endline(ptr %r1)
  ret i32 %r2
}


define i32 @main() {
  %result = call i32 @mucaml_main(i32 0)
  ret i32 %result
}