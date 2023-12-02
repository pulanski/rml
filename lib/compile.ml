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

let compile_to_target ir target output_file =
  let oc = open_out output_file in
  let code = match target with
  | "c" -> emit_c ir
    (* | "mlir" -> emit_mlir ir
    | "js" -> emit_js ir *)
    | _ -> failwith "Invalid target"
  in
  output_string oc code;
  close_out oc

let compile input_file output_config =
  let ast = time_phase "Parse" (fun () -> parse_program input_file) in
  let () = time_phase "Sema" (fun () -> type_check_program ast) in
  let ir = time_phase "IR Lowering" (fun () -> generate_ir ast) in

  if output_config.output_mlir then
    time_phase "MLIR Codegen" (fun () -> compile_to_target ir "mlir" (Filename.chop_suffix input_file ".rs" ^ ".mlir"));
  if output_config.output_js then
    time_phase "JS Codegen" (fun () -> compile_to_target ir "js" (Filename.chop_suffix input_file ".rs" ^ ".js"));
  if output_config.output_c then
    time_phase "C Codegen" (fun () -> compile_to_target ir "c" (Filename.chop_suffix input_file ".rs" ^ ".c"));


  (* Displaying compilation analytics and metrics *)
  report_timings ()

(* let compile input_file output_config =
  let start_time = gettimeofday () in
  let timings = ref [] in

  let time_phase phase_name f =
    let phase_start = gettimeofday () in
    let result = f () in
    let phase_end = gettimeofday () in
    timings := (phase_name, phase_end -. phase_start) :: !timings;
    result
  in

  let channel = open_in input_file in
  try
    Printf.printf "Compiling: %s\n" input_file;
    let lexbuf = Lexing.from_channel channel in

    Printf.printf "Parse ...\n";
    let ast = time_phase "Lexing and Parsing" (fun () -> Parser.program Lexer.token lexbuf) in

    Printf.printf "Sema ...\n";
    let () = time_phase "Semantic Analysis" (fun () -> analyze_program ast) in

    Printf.printf "IRGen ...\n";
    let ir = time_phase "IR Generation" (fun () -> generate_ir ast) in

    if output_config.output_mlir then begin
      Printf.printf "MLIRGen ...\n";
      let mlir_code = time_phase "MLIR Code Emission" (fun () -> emit_mlir ir) in

      Printf.printf "CodeGen ...\n";
      let output_file = Filename.chop_suffix input_file ".rs" ^ ".mlir" in
      let oc = open_out output_file in
      output_string oc mlir_code;
      close_out oc;

      (* Displaying compilation analytics and metrics *)
      let total_time = Unix.gettimeofday () -. start_time in
      Printf.printf "Compiled %s in %s\n" output_file (format_time total_time);
      Printf.printf "\nCompilation Analytics and Metrics:\n";
      List.iter (fun (phase, time) ->
        Printf.printf "%s: %s\n" phase (format_time time)
      ) !timings;

      let total_time_formatted = format_time total_time in
      Printf.printf "\nTotal Compilation Time: %s\n" total_time_formatted;

      Printf.printf "\nTime Distribution Bar Chart:\n";
      List.iter (fun (phase, time) ->
        let proportion = time /. total_time *. 50.0 in
        Printf.printf "%s: %s\n" phase (String.make (int_of_float proportion) '#')
      ) !timings;
    end;

    if output_config.output_js then begin
      Printf.printf "JSGen ...\n";
      let js_code = time_phase "JS Code Emission" (fun () -> emit_js ir) in

      Printf.printf "CodeGen ...\n";
      let output_file = Filename.chop_suffix input_file ".rs" ^ ".js" in
      let oc = open_out output_file in
      output_string oc js_code;
      close_out oc;

      (* Displaying compilation analytics and metrics *)
      let total_time = Unix.gettimeofday () -. start_time in
      Printf.printf "Compiled %s in %s\n" output_file (format_time total_time);
      Printf.printf "\nCompilation Analytics and Metrics:\n";
      List.iter (fun (phase, time) ->
        Printf.printf "%s: %s\n" phase (format_time time)
      ) !timings;

      let total_time_formatted = format_time total_time in
      Printf.printf "\nTotal Compilation Time: %s\n" total_time_formatted;

      Printf.printf "\nTime Distribution Bar Chart:\n";
      List.iter (fun (phase, time) ->
        let proportion = time /. total_time *. 50.0 in
        Printf.printf "%s: %s\n" phase (String.make (int_of_float proportion) '#')
      ) !timings;
    end;

    if output_config.output_c then begin
      Printf.printf "CGen ...\n";
      let c_code = time_phase "C Code Emission" (fun () -> emit_c ir) in

      Printf.printf "CodeGen ...\n";
      let output_file = Filename.chop_suffix input_file ".rs" ^ ".c" in
      let oc = open_out output_file in
      output_string oc c_code;
      close_out oc;

      (* Displaying compilation analytics and metrics *)
      let total_time = Unix.gettimeofday () -. start_time in
      Printf.printf "Compiled %s in %s\n" output_file (format_time total_time);
      Printf.printf "\nCompilation Analytics and Metrics:\n";
      List.iter (fun (phase, time) ->
        Printf.printf "%s: %s\n" phase (format_time time)
      ) !timings;

      let total_time_formatted = format_time total_time in
      Printf.printf "\nTotal Compilation Time: %s\n" total_time_formatted;

      Printf.printf "\nTime Distribution Bar Chart:\n";
      List.iter (fun (phase, time) ->
        let proportion = time /. total_time *. 50.0 in
        Printf.printf "%s: %s\n" phase (String.make (int_of_float proportion) '#')
      ) !timings;
    end;
  with
  | e ->
      Printf.eprintf "An error occurred: %s\n" (Printexc.to_string e);
      close_in_noerr channel;
      raise e *)