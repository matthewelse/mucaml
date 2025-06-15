open! Core
module E = MenhirLib.ErrorReports

module Backend_demo = struct
  let main =
    Command.basic
      ~summary:"mucaml backend demo"
      [%map_open.Command
        let () = return () in
        fun () ->
          let dsl = Arm_dsl.create () in
          Emit.Program.emit
            [ { name = "mucaml_main"; args = (); body = Return 42 } ]
            dsl
            ~target:CortexM33;
          print_endline (Arm_dsl.to_string dsl)]
  ;;
end

module Frontend_demo = struct
  let main =
    Command.basic
      ~summary:"mucaml frontend demo"
      [%map_open.Command
        let () = return () in
        fun () ->
          while
            match LNoise.linenoise "> " with
            | None -> false
            | Some input ->
              LNoise.history_add input |> Result.ok_or_failwith;
              (match Parse.parse_toplevel input ~filename:"<stdin>" with
               | Ok ast ->
                 Ast.pprint_prog ast;
                 true
               | Error () -> true)
          do
            ()
          done]
  ;;
end

let main () =
  Command.group
    ~summary:"mucaml"
    [ "backend-demo", Backend_demo.main; "frontend-demo", Frontend_demo.main ]
  |> Command_unix.run
;;
