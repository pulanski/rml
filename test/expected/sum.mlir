module {
  toy.func @multiply_transpose(%arg0: tensor<*xf64>, %arg1: tensor<*xf64>) -> tensor<*xf64> {
    %0 = toy.generic_call @transpose(%arg0) : (tensor<*xf64>) -> tensor<*xf64>
    %1 = toy.generic_call @transpose(%arg1) : (tensor<*xf64>) -> tensor<*xf64>
    %2 = toy.mul %0, %1 : tensor<*xf64>
    toy.return %2 : tensor<*xf64>
  }
  toy.func @main() {
    %0 = toy.constant dense<[[1.000000e+00, 2.000000e+00, 3.000000e+00], [4.000000e+00, 5.000000e+00, 6.000000e+00]]> : tensor<2x3xf64>
    %1 = toy.constant dense<[1.000000e+00, 2.000000e+00, 3.000000e+00, 4.000000e+00, 5.000000e+00, 6.000000e+00]> : tensor<6xf64>
    %2 = toy.generic_call @multiply_transpose(%0, %1) : (tensor<2x3xf64>, tensor<6xf64>) -> tensor<*xf64>
    %3 = toy.generic_call @multiply_transpose(%1, %0) : (tensor<6xf64>, tensor<2x3xf64>) -> tensor<*xf64>
    toy.print %3 : tensor<*xf64>
    toy.return
  }
}