open! Core
open! Import

let llvm_type_of_mirl_type = function
  | Mirl.Type.I32 -> "i32"
  | Mirl.Type.I64 -> "i64"
  | Mirl.Type.Ptr -> "ptr"
;;

let generate_module_header ~triple =
  let target_triple =
    match triple.Triple.architecture with
    | Arm64 -> "aarch64-unknown-linux-gnu"
    | _ -> failwith "Unsupported architecture for LLVM backend"
  in
  [%string
    {|
target triple = "%{target_triple}"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
|}]
;;

let generate_external (ext : Mirl.External.t) =
  let arg_types =
    List.map ext.arg_types ~f:llvm_type_of_mirl_type |> String.concat ~sep:", "
  in
  let return_type = llvm_type_of_mirl_type ext.return_type in
  [%string {|declare %{return_type} @%{ext.c_name}(%{arg_types})|}]
;;

let generate_global_constants (constants : string iarray) =
  Iarray.to_list constants
  |> List.mapi ~f:(fun i content ->
    [%string
      {|@global_const_%{i#Int} = private unnamed_addr constant [%{String.length content + 1#Int} x i8] c"%{content}\00"|}])
  |> String.concat ~sep:"\n"
;;

let virtual_reg_name (vr : Mirl.Virtual_register.t) =
  [%string "%r%{Mirl.Virtual_register.to_int_exn vr#Int}"]
;;

let label_name (label : Mirl.Label.t) = [%string "label_%{label#Mirl.Label}"]

let generate_instruction ~global_constants (instr : Mirl.Instruction.t) =
  match instr with
  | Add { dst; src1; src2 } ->
    [%string
      "%{virtual_reg_name dst} = add i64 %{virtual_reg_name src1}, %{virtual_reg_name \
       src2}"]
  | Sub { dst; src1; src2 } ->
    [%string
      "%{virtual_reg_name dst} = sub i64 %{virtual_reg_name src1}, %{virtual_reg_name \
       src2}"]
  | Add_with_carry { dst; src1; src2 } ->
    (* LLVM doesn't have direct add-with-carry, simulate with larger types *)
    [%string
      "%{virtual_reg_name dst} = add i64 %{virtual_reg_name src1}, %{virtual_reg_name \
       src2} ; TODO: add-with-carry"]
  | Sub_with_carry { dst; src1; src2 } ->
    [%string
      "%{virtual_reg_name dst} = sub i64 %{virtual_reg_name src1}, %{virtual_reg_name \
       src2} ; TODO: sub-with-carry"]
  | Set { dst; value } -> [%string "%{virtual_reg_name dst} = add i64 0, %{value#Int}"]
  | Load_global_constant { dst; constant } ->
    let const_id = Mirl.Global_constant.to_int_exn constant in
    let const_str = Iarray.get global_constants const_id in
    let array_size = String.length const_str + 1 in
    [%string
      "%{virtual_reg_name dst} = getelementptr [%{array_size#Int} x i8], ptr \
       @global_const_%{const_id#Int}, i64 0, i64 0"]
  | Mov { dst; src } ->
    [%string "%{virtual_reg_name dst} = add i64 %{virtual_reg_name src}, 0"]
  | C_call { dst; func; args } ->
    let arg_list =
      List.map args ~f:(fun arg -> [%string "ptr %{virtual_reg_name arg}"])
      |> String.concat ~sep:", "
    in
    [%string "%{virtual_reg_name dst} = call i32 @%{func}(%{arg_list})"]
  | Jump { target } -> [%string "br label %%{label_name target}"]
  | Return args ->
    (match args with
     | [] -> "ret void"
     | [ arg ] -> [%string "ret i32 %{virtual_reg_name arg}"]
     | _ -> failwith "Multiple return values not supported in LLVM IR generation")
  | Branch { condition; target } ->
    [%string
      "br i1 %{virtual_reg_name condition}, label %%{label_name target}, label %%next"]
;;

let generate_block ~global_constants (block : Mirl.Block.t) ~is_first =
  let label_decl = if is_first then "" else [%string "%{label_name block.label}:\n"] in
  let instructions =
    Iarray.to_list block.instructions
    |> List.map ~f:(fun instr -> "  " ^ generate_instruction ~global_constants instr)
    |> String.concat ~sep:"\n"
  in
  label_decl ^ instructions
;;

let generate_function ~global_constants (func : Mirl.Function.t) =
  let param_list =
    List.map func.params ~f:(fun (_, vr, ty) ->
      [%string "%{llvm_type_of_mirl_type ty} %{virtual_reg_name vr}"])
    |> String.concat ~sep:", "
  in
  let return_type = "i32" in
  (* Simplify for now *)
  let header = [%string "define %{return_type} @%{func.name}(%{param_list}) {"] in
  let blocks =
    Iarray.to_list func.body
    |> List.mapi ~f:(fun i block ->
      generate_block ~global_constants block ~is_first:(i = 0))
    |> String.concat ~sep:"\n"
  in
  header ^ "\n" ^ blocks ^ "\n}"
;;

let generate (mirl : Mirl.t) ~triple =
  let header = generate_module_header ~triple in
  let constants = generate_global_constants mirl.global_constants in
  let externals = List.map mirl.externs ~f:generate_external |> String.concat ~sep:"\n" in
  let functions =
    List.map mirl.functions ~f:(generate_function ~global_constants:mirl.global_constants)
    |> String.concat ~sep:"\n\n"
  in
  let main_wrapper =
    {|
define i32 @main() {
  %result = call i32 @mucaml_main(i32 0)
  ret i32 %result
}|}
  in
  String.concat ~sep:"\n\n" [ header; constants; externals; functions; main_wrapper ]
  |> Result.return
;;
