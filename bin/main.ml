open Rml.Compile

let () =
  let input_file = "test/in/sum.rml" in
  let output_config = {
    output_js = true;
    output_c = true;
    output_mlir = true;
  } in
  compile input_file output_config