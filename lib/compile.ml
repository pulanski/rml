open Sema
open Irgen
(* open Mlirgen
open Jsgen *)
open Cgen
open Util

type compilation_config = {
  output_js: bool;
  output_c: bool;
  output_mlir: bool;
}

let parse_program input_file =
  let channel = open_in input_file in
  let lexbuf = Lexing.from_channel channel in
  Parser.program Lexer.token lexbuf

let compile_to_target ir target base_output_file input_file =
  match target with
  | "c" ->
      (* Generate header file *)
      let header_code = emit_c_header ir input_file in
      let header_file = base_output_file ^ ".h" in
      let oc_header = open_out header_file in
      output_string oc_header header_code;
      close_out oc_header;

      (* Generate source file *)
      let source_code = emit_c_source ir input_file in
      let source_file = base_output_file ^ ".c" in
      let oc_source = open_out source_file in
      output_string oc_source source_code;
      close_out oc_source;

  (* Other targets ... *)
  (* | "mlir" -> emit_mlir ir
  | "js" -> emit_js ir *)
  | _ -> failwith "Invalid target"

let compile input_file output_config =
  let ast = time_phase "Parse" (fun () -> parse_program input_file) in
  let () = time_phase "Sema" (fun () -> type_check_program ast) in
  let ir = time_phase "IR Lowering" (fun () -> generate_ir ast) in

  (* if output_config.output_mlir then
    time_phase "MLIR Codegen" (fun () -> compile_to_target ir "mlir" (Filename.chop_suffix input_file ".rs" ^ ".mlir")); *)
  (* if output_config.output_js then
    time_phase "JS Codegen" (fun () -> compile_to_target ir "js" (Filename.chop_suffix input_file ".rs" ^ ".js")); *)
  if output_config.output_c then
  time_phase "C Codegen" (fun () -> compile_to_target ir "c" (Filename.chop_suffix input_file ".rs") input_file);

  (* Displaying compilation analytics and metrics *)
  report_timings ()