// Define a function in MLIR.
func.func @add(%arg0: i32, %arg1: i32) -> i32 {
  %0 = arith.addi %arg0, %arg1 : i32
  return %0 : i32
}

func.func @main() {
  %0 = arith.constant 2. : f64
  %1 = arith.constant 3. : f64
  %2 = arith.addf %0, %1 : f64

  return
}