open Cmdliner
(* open Sema
open Mlirgen
open Jsgen *)
open Irgen
open Cgen
open Util

let parse_program input_file =
  let channel = open_in input_file in
  let lexbuf = Lexing.from_channel channel in
  Parser.program Lexer.token lexbuf

let failed_to_infer_output_type_message () =
  "\n[WARNING] Failed to infer output type\n\n" ^
  "Specify output types with the --emit flag (e.g., --emit c,js,mlir)\n" ^
  "Valid output types are: c, js, mlir\n\n" ^
  "Example:\n  rml --emit c,js,mlir file1.rs\n\n" ^
  "This will generate C, JS, and MLIR code for file1.rs\n\n" ^
  "Note: If you specify an output file with the -o flag, the output type will be inferred from the file extension.\n\n" ^
  "      For example, if you specify -o file1.js, the output type will be JS.\n" ^
  "      If you specify -o file1.c, the output type will be C.\n" ^
  "      If you specify -o file1.mlir, the output type will be MLIR.\n"


let compile_to_target ir target base_output_file input_file =
  print_endline ("Compiling to target: " ^ target);
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

type compilation_config = {
  output_js: bool;
  output_c: bool;
  output_mlir: bool;
}

let compile_command input_files output_file emit_types _include_dirs _verbose =
  let compile_one_file input_file =
    let base_output_file = match output_file with
      | Some s -> s
      | None -> Filename.remove_extension input_file
    in
    let output_config = {
      output_js = List.exists (fun t -> t = "js") emit_types || Filename.check_suffix base_output_file ".js";
      output_c = List.exists (fun t -> t = "c") emit_types || Filename.check_suffix base_output_file ".c";
      output_mlir = List.exists (fun t -> t = "mlir") emit_types || Filename.check_suffix base_output_file ".mlir";
    } in
    let is_codegen_specified = output_config.output_c || output_config.output_js || output_config.output_mlir in
    if not is_codegen_specified then begin
      print_endline (failed_to_infer_output_type_message ());
    end;


    print_endline ("Compiling file: " ^ input_file ^ "...");
    let ast = time_phase "Parse" (fun () -> parse_program input_file) in
    let ir = time_phase "IR Lowering" (fun () -> generate_ir ast) in
    if output_config.output_c then
      time_phase "C Codegen" (fun () -> compile_to_target ir "c" base_output_file input_file);
    if output_config.output_js then
      (* print_endline "Generating JS code"; *)
      time_phase "JS Codegen" (fun () -> compile_to_target ir "js" base_output_file input_file);
    if output_config.output_mlir then
      (* print_endline "Generating MLIR code"; *)
      time_phase "MLIR Codegen" (fun () -> compile_to_target ir "mlir" base_output_file input_file);

    report_timings ()  (* This can be moved outside the loop if we prefer aggregate metrics *)
  in
  List.iter compile_one_file input_files

let compile_term =
  let input_files =
    let doc = "List of input files to compile." in
    Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)
  in
  let output_file =
    let doc = "Set the output file name." in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)
  in
  let emit_types =
    let doc = "Set the types of output to generate (e.g., 'c,mlir,js')." in
    let emit_types_conv = Arg.list ~sep:',' Arg.string in
    Arg.(value & opt emit_types_conv [] & info ["emit"] ~docv:"EMIT_TYPES" ~doc)
  in
  let include_dirs =
    let doc = "Include directories." in
    Arg.(value & opt_all dir [] & info ["I"] ~docv:"DIR" ~doc)
  in
  let verbose =
    let doc = "Enable verbose mode." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  Term.(const compile_command $ input_files $ output_file $ emit_types $ include_dirs $ verbose)

let compile_cmd : unit Cmdliner.Cmd.t =
  let doc = "Rust to C/JS Transpiler" in
  let info = Cmd.info "rml" ~doc in
  Cmd.v info compile_term