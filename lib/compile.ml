open Sema
open Irgen
open Codegen

let compile_to_mlir input_file output_file =
  let channel = open_in input_file in
  try
    (* Read input file *)
    let lexbuf = Lexing.from_channel channel in

    (* Lexing and Parsing *)
    let ast = Parser.program Lexer.token lexbuf in

    (* Semantic Analysis *)
    analyze_program ast;

    (* Intermediate Representation Generation *)
    let ir = generate_ir ast in

    (* MLIR Code Emission *)
    let mlir_code = emit_mlir ir in

    (* Write MLIR to output file *)
    let oc = open_out output_file in
    output_string oc mlir_code;
    close_out oc;

    (* Clean up *)
    close_in channel;
  with
  | e ->
      close_in channel;
      raise e
