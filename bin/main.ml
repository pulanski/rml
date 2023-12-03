open Rml.Compile
open Cmdliner

let main () = exit (Cmd.eval (compile_cmd))

let () = main ()

(* open Rml.Cli
open Rml.Compile

let main () =
  let args = parse_args () in
  if args.show_help then show_help ()
  else if args.show_version then show_version ()
  else compile_files args *)


(* open Rml.Compile

let () =
  let input_file = "test/in/sum.rs" in
  let output_config = {
    output_js = false;
    output_c = true;
    output_mlir = false;
  } in
  compile input_file output_config *)