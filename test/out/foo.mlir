module {
  func.func @example(%arg0: memref<?xf32, strided<[1]>>, %arg1: memref<?xvector<4xf32>, strided<[2], offset: 1>>) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %dim = memref.dim %arg0, %c0 : memref<?xf32, strided<[1]>>
    scf.for %arg2 = %c0 to %dim step %c1 {
      %0 = memref.load %arg0[%arg2] : memref<?xf32, strided<[1]>>
      %1 = memref.load %arg1[%arg2] : memref<?xvector<4xf32>, strided<[2], offset: 1>>
      %2 = "some_compute"(%0, %1) : (f32, vector<4xf32>) -> vector<4xf32>
      memref.store %2, %arg1[%arg2] : memref<?xvector<4xf32>, strided<[2], offset: 1>>
    }
    return
  }
}

