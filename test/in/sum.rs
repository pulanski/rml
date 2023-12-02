struct Foo {
    a: u32,
    b: u32,
}

enum Bar {
    A,
    B,
}

fn main() -> u32 {
    let e = 42;
    return e;
} // TODO: improve scope analysis

// fn main() -> void {
//     if true {
//         let mut f = 2;
//     }

//     let a = 2;
//     let b = 20;
//     // let c = a + b;
//     // TODO: add support for function calls
//     // let d = c - foo();
// }

// let foo = Foo { a: 2, b: 20 };
// let bar = Bar::A;
// let c = foo.a + foo.b;
// let d = c - foo.a;
// let e = d * foo.b;

// fn foo() -> u32 {
//     let a = 2;
//     let b = 20;
//     let c = 3;
//     let d = 17;
//     // let e = c + d;
//     // let f = a - b;
//     // let g = f * e;
//     // return g;
// }

// while true {
//     let f = 2;
// }

// loop {
//     // game loop
//     let life = 42;

//     if life < 0 {
//         // break;
//         let life = 42;
//     }

//     // life = life - 1;
// }

// // for i in 0..10 {
// //   let f = 2;
// // }

// if true {
//     let f = 2;
// } else {
//     let f = 3;
// }

// if true {
//     let f = 2;
// } else if false {
//     let f = 3;
// } else {
//     let f = 4;
// }
