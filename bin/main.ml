open Rml.Compile

let () =
  let input_file = "test/in/sum.rml" in
  let output_config = {
    output_js = false;
    output_c = true;
    output_mlir = false;
  } in
  compile input_file output_config