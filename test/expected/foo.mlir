module {

func.func @main() {
  %0 = arith.constant 2. : f64
  %1 = arith.constant 3. : f64
  %2 = arith.addf %0, %1 : f64
  return
}

func.func @foo() {
  %0 = arith.constant 2. : f64
  %1 = arith.constant 20. : f64
  %2 = arith.constant 3. : f64
  %3 = arith.constant 17. : f64
  %4 = arith.addf %2, %3 : f64 // 3 + 17
  %5 = arith.subf %0, %1 : f64 // 2 - 20
  %6 = arith.mulf %5, %4 : f64 // (2 - 20) * (3 + 17)
  return
}

}