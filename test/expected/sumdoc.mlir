module {
  rml.func @multiply_transpose(%arg0: tensor<*xf64>, %arg1: tensor<*xf64>) -> tensor<*xf64> {
    %0 = rml.transpose(%arg0 : tensor<*xf64>) to tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":5:10)
    %1 = rml.transpose(%arg1 : tensor<*xf64>) to tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":5:25)
    %2 = rml.mul %0, %1 : tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":5:25)
    rml.return %2 : tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":5:3)
  } loc("test/Examples/rml/Ch2/codegen.rml":4:1)
  rml.func @main() {
    %0 = rml.constant dense<[[1.000000e+00, 2.000000e+00, 3.000000e+00], [4.000000e+00, 5.000000e+00, 6.000000e+00]]> : tensor<2x3xf64> loc("test/Examples/rml/Ch2/codegen.rml":9:17)
    %1 = rml.reshape(%0 : tensor<2x3xf64>) to tensor<2x3xf64> loc("test/Examples/rml/Ch2/codegen.rml":9:3)
    %2 = rml.constant dense<[1.000000e+00, 2.000000e+00, 3.000000e+00, 4.000000e+00, 5.000000e+00, 6.000000e+00]> : tensor<6xf64> loc("test/Examples/rml/Ch2/codegen.rml":10:17)
    %3 = rml.reshape(%2 : tensor<6xf64>) to tensor<2x3xf64> loc("test/Examples/rml/Ch2/codegen.rml":10:3)
    %4 = rml.generic_call @multiply_transpose(%1, %3) : (tensor<2x3xf64>, tensor<2x3xf64>) -> tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":11:11)
    %5 = rml.generic_call @multiply_transpose(%3, %1) : (tensor<2x3xf64>, tensor<2x3xf64>) -> tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":12:11)
    rml.print %5 : tensor<*xf64> loc("test/Examples/rml/Ch2/codegen.rml":13:3)
    rml.return loc("test/Examples/rml/Ch2/codegen.rml":8:1)
  } loc("test/Examples/rml/Ch2/codegen.rml":8:1)
} loc(unknown)