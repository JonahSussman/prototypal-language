use std::io::Write;
// use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
// use std::sync::Mutex;

type Ptr<T> = Rc<RefCell<T>>;

struct Env {}

enum Exp {
  Number(f64),
  String(String),
  Boolean(bool),
  Symbol(String),
  List(Vec<Ptr<Exp>>),
  // Thing {
  //  table: &HashMap<Exp, Pexp>,
  //  meta: Pexp
  // },
  // Call {
  //   name: Pexp,
  //   args: Vec<Pexp>
  // },
  // Procedure {
  //   params: Vec<Pexp>,
  //   body: Vec<Pexp>,
  //   env: Penv
  // },
  // Primitive {
  //   fun: Vec<Box<dyn Fn(Penv, Vec<Pexp>) -> Pexp>>,
  //   tco: bool
  // },
  Special
}
// const nil : Pexp = Pexp(Cell::new(Box::new(Exp::Special)));

// unsafe impl Sync for Pexp {}

// enum MyEnum {
//   A(A),
//   B(B)
// }

// struct A {
//   x: i32,
//   y: i32
// }

// struct B {
//   name: String
// }

fn main() {
  let repl = true;

  // let a = A { x: 56, y: 400 };
  // println!("{} {}", a.x, a.y);
  // let b = B { name: String::from("Hello!") };

  let a = Box::new(Exp::Number(40.0));
  let b = Box::new(Exp::Number(50.0));

  let mut vec = Vec::new();
  vec.push(a);
  vec.push(b);

  // a 

  // println!("{:?}", vec);

  // loop {
  //   let mut s = String::new();

  //   if repl {
  //     print!("lot> ");
  //     std::io::stdout().flush().unwrap();
  //     std::io::stdin().read_line(&mut s).unwrap();
  //     s.pop();
  //   } else {
  //     break;
  //   }

  //   println!("No, you're a {}", s);
  // }
}