open Rml

let () =
  let input_file = "test/in/sum.rml" in
  let output_file = "test/out/sum.mlir" in
  Compile.compile_to_mlir input_file output_file