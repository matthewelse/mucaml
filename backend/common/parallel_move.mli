open! Core
open! Import

val parallel_move
  :  src:'reg array @ local
  -> dst:'reg iarray @ local
  -> tmp:('reg -> 'reg)
  -> ('reg * 'reg) list @ local
