open! Core

type 'a t = ( :: ) of 'a * 'a list

let to_list (hd :: tl) : _ list = hd :: tl
