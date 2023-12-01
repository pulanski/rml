open Sema
open Irgen
open Codegen
open Unix

let format_time duration =
  match duration with
  | t when t >= 60. -> Printf.sprintf "%.2f m" (t /. 60.)
  | t when t >= 1. -> Printf.sprintf "%.2f s" t
  | t when t >= 0.001 -> Printf.sprintf "%.2f ms" (t *. 1000.)
  | t when t >= 0.000001 -> Printf.sprintf "%.2f μs" (t *. 1_000_000.)
  | t -> Printf.sprintf "%.2f nanoseconds" (t *. 1_000_000_000.)

let compile_to_mlir input_file output_file =
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

    Printf.printf "MLIRGen ...\n";
    let mlir_code = time_phase "MLIR Code Emission" (fun () -> emit_mlir ir) in

    Printf.printf "CodeGen ...\n";
    let oc = open_out output_file in
    output_string oc mlir_code;
    close_out oc;

    (* Clean up *)

    close_in channel;

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

  with
  | e ->
      Printf.eprintf "An error occurred: %s\n" (Printexc.to_string e);
      close_in_noerr channel;
      raise e

