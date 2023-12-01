module {
  rml.func @multiply_transpose(%arg0: tensor<*xf64>, %arg1: tensor<*xf64>) -> tensor<*xf64> {
    %0 = rml.generic_call @transpose(%arg0) : (tensor<*xf64>) -> tensor<*xf64>
    %1 = rml.generic_call @transpose(%arg1) : (tensor<*xf64>) -> tensor<*xf64>
    %2 = rml.mul %0, %1 : tensor<*xf64>
    rml.return %2 : tensor<*xf64>
  }
  rml.func @main() {
    %0 = rml.constant dense<[[1.000000e+00, 2.000000e+00, 3.000000e+00], [4.000000e+00, 5.000000e+00, 6.000000e+00]]> : tensor<2x3xf64>
    %1 = rml.constant dense<[1.000000e+00, 2.000000e+00, 3.000000e+00, 4.000000e+00, 5.000000e+00, 6.000000e+00]> : tensor<6xf64>
    %2 = rml.generic_call @multiply_transpose(%0, %1) : (tensor<2x3xf64>, tensor<6xf64>) -> tensor<*xf64>
    %3 = rml.generic_call @multiply_transpose(%1, %0) : (tensor<6xf64>, tensor<2x3xf64>) -> tensor<*xf64>
    rml.print %3 : tensor<*xf64>
    rml.return
  }
}