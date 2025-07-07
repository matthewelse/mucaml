open! Core

module Recovery = struct
  (* For better error messages, we use Menhir's table backend. *)

  module E = MenhirLib.ErrorReports
  module L = MenhirLib.LexerUtil
  module I = Recovery_parser.MenhirInterpreter

  let succeed _v = assert false

  let env checkpoint =
    match checkpoint with
    | I.HandlingError env -> env
    | _ -> assert false
  ;;

  let state checkpoint : int =
    match I.top (env checkpoint) with
    | Some (I.Element (s, _, _, _)) -> I.number s
    | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0
  ;;

  let fail buffer (checkpoint : _ I.checkpoint) ~file_id =
    let start_pos, end_pos = E.last buffer in
    let state = state checkpoint in
    let diagnostic : Grace.Diagnostic.t =
      let open Grace in
      { severity = Error
      ; message = (fun fmt -> Format.fprintf fmt "Syntax error (%d)" state)
      ; labels =
          [ Diagnostic.Label.primary
              ~id:file_id
              ~range:(Grace.Range.of_lex start_pos end_pos)
              (fun ppf ->
                 let message = Parser_messages.message state in
                 Format.pp_print_string ppf message)
          ]
      ; notes = []
      }
    in
    Error diagnostic
  ;;

  let parse text ~file_id =
    let lexbuf = Lexing.from_string text in
    let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint = Recovery_parser.Incremental.prog lexbuf.lex_curr_p in
    I.loop_handle succeed (fail buffer ~file_id) supplier checkpoint
  ;;
end

let parse_toplevel text ~file_id =
  let lexer = Lexing.from_string text in
  try Ok (Parser.prog Lexer.read lexer) with
  | Parser.Error -> Recovery.parse text ~file_id
  | Lexer.Error message ->
    let start_pos = Lexing.lexeme_start_p lexer in
    let end_pos = Lexing.lexeme_end_p lexer in
    let diagnostic : Grace.Diagnostic.t =
      let open Grace in
      { severity = Error
      ; message = (fun fmt -> Format.fprintf fmt "Invalid character")
      ; labels =
          [ Diagnostic.Label.primary
              ~id:file_id
              ~range:(Grace.Range.of_lex start_pos end_pos)
              (fun ppf -> Format.pp_print_string ppf message)
          ]
      ; notes = []
      }
    in
    Error diagnostic
;;
