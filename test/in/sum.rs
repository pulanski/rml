use std::collections::hash_map;
// use std::collections::hash_map::{self, HashMap};

struct Foo {
    a: u32,
    b: u32,
}

enum Bar {
    A,
    B,
}

mod baz {
    fn foo() -> u32 {
        return 42;
    }

    fn sum(a: u32, b: u32) -> u32 {
        for i in 3 {
            // if i == 5 {
            break;
            // }
        }
    }
}

fn add(a: u32, b: u32) -> u32 {
    // a + b // TODO: add support for returning expressions
    return a + b;
}

fn main() -> u32 {
    loop {
        // game loop
        let life = 42;

        if life < 0 {
            // break;
            let life = 42;
        }

        // life = life - 1;
    }

    let e = 42;
    return e;
} // TODO: improve scope analysis

// TODO: add support for sema and codegen on traits
trait Trait {
    // fn foo(&self) -> u32; // TODO: add support for references
    fn foo() -> u32;
}

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
