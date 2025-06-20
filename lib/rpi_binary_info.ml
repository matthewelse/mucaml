open! Core

module Entry = struct
  module Id = struct
    type t = int

    (* Used to note the program name - use with StringEntry *)
    let rp_program_name = 0x02031c86

    (* Used to note the program version - use with StringEntry *)
    let rp_program_version_string = 0x11a9bc3a

    (* Used to note the program build date - use with StringEntry *)
    let rp_program_build_date_string = 0x9da22254

    (* Used to note the size of the binary - use with IntegerEntry *)
    let rp_binary_end = 0x68f465de

    (* Used to note a URL for the program - use with StringEntry *)
    let rp_program_url = 0x1856239a

    (* Used to note a description of the program - use with StringEntry *)
    let rp_program_description = 0xb6a07c19

    (* Used to note some feature of the program - use with StringEntry *)
    let rp_program_feature = 0xa1f4b453

    (* Used to note some whether this was a Debug or Release build - use with StringEntry *)
    let rp_program_build_attribute = 0x4275f0d3

    (* Used to note the Pico SDK version used - use with StringEntry *)
    let rp_sdk_version = 0x5360b3ab

    (* Used to note which board this program targets - use with StringEntry *)
    let rp_pico_board = 0xb63cffbb

    (* Used to note which `boot2` image this program uses - use with StringEntry *)
    let rp_boot2_name = 0x7f8882e1
  end

  module Type = struct
    type _ t =
      (* Raw data *)
      | Raw : Nothing.t t
      (* Data with a size *)
      | SizedData : Nothing.t t
      (* A list of binary data *)
      | BinaryInfoListZeroTerminated : Nothing.t t
      (* A BSON encoded blob *)
      | Bson : Nothing.t t
      (* An Integer with an ID *)
      | IdAndInt : (Id.t * int) t
      (* An Integer (pointer to somewhere in the binary) with an ID *)
      | IdAndIntLabel : (Id.t * string) t
      (* A string with an Id *)
      | IdAndString : (Id.t * string) t
      (* A block device *)
      | BlockDevice : Nothing.t t
      (* GPIO pins, with their function *)
      | PinsWithFunction : Nothing.t t
      (* GPIO pins, with their name *)
      | PinsWithName : Nothing.t t
      (* GPIO pins, with multiple names? *)
      | PinsWithNames : Nothing.t t

    let id : type a. a t -> int = function
      | Raw -> 1
      | SizedData -> 2
      | BinaryInfoListZeroTerminated -> 3
      | Bson -> 4
      | IdAndInt | IdAndIntLabel -> 5
      | IdAndString -> 6
      | BlockDevice -> 7
      | PinsWithFunction -> 8
      | PinsWithName -> 9
      | PinsWithNames -> 10
    ;;
  end

  type t =
    | T :
        { type_ : 'a Type.t
        ; namespace : char * char
        ; value : 'a
        }
        -> t

  let generate_assembly (T { type_; namespace = nl, nr; value }) =
    match type_ with
    | IdAndString ->
      let id, value = value in
      (* TODO: escape [value] properly *)
      [%string
        {|.hword %{Type.id type_#Int.Hex}
.byte '%{nl#Char}', '%{nr#Char}'
.word %{id#Int.Hex}
.word . + 4
.ascii "%{value#String}"|}]
    | IdAndInt ->
      let id, value = value in
      [%string
        {|.hword %{Type.id type_#Int.Hex}
.byte '%{nl#Char}', '%{nr#Char}'
.word %{id#Int.Hex}
.word %{value#Int.Hex}
|}]
    | IdAndIntLabel ->
      let id, value = value in
      [%string
        {|.hword %{Type.id type_#Int.Hex}
.byte '%{nl#Char}', '%{nr#Char}'
.word %{id#Int.Hex}
.word %{value}
|}]
    | _ -> failwith "Not supported."
  ;;
end

let generate_assembly t =
  let entries =
    List.mapi t ~f:(fun i entry ->
      [%string "bi_entry_%{i#Int}:\n"] ^ Entry.generate_assembly entry)
    |> String.concat ~sep:"\n\n"
  in
  let entry_pointers =
    List.mapi t ~f:(fun i _ -> [%string ".word bi_entry_%{i#Int}"])
    |> String.concat ~sep:"\n"
  in
  [%string
    {|.section .rodata, "a"
%{entries}

mapping_table:
  // TODO: I think we don't actually need this unless we want to look at this data at
  // runtime.
  .word 0x00000000 
  .word 0x00000000 
  .word 0x00000000 
  
.section .bi_entries, "a"
%{entry_pointers}

.section .boot_info, "a"
.word 0x7188ebf2
.word __bi_entries_start 
.word __bi_entries_end 
.word mapping_table 
.word 0xe71aa390
  |}]
;;
