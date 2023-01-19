// use std::{
//     fmt::Display,
//     ops::{Add, BitXor, Div, Mul, Rem, Sub},
// };

// use nyxc_ast::ast::{Expression, FunctionParameters, Program};

// use crate::environment::Environment;

// #[derive(Debug, PartialEq, Clone)]
// pub enum Object {
//     Integer(i64),
//     Float(f64),
//     Bool(bool),
//     Str(String),
//     Return(Box<Object>),
//     Function {
//         body: Expression,
//         name: String,
//         env: Environment,
//         parameters: FunctionParameters,
//     },
//     Null,
// }

// impl Add for Object {
//     type Output = Object;

//     fn add(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Object::Integer(a), Object::Integer(b)) => Object::Integer(a + b),
//             (Object::Float(a), Object::Float(b)) => Object::Float(a + b),
//             (Object::Str(a), Object::Str(b)) => Object::Str(format!("{a}{b}")),
//             (Object::Null, Object::Null) => Object::Null,
//             _ => Object::Null,
//         }
//     }
// }

// impl Sub for Object {
//     type Output = Object;

//     fn sub(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Object::Integer(a), Object::Integer(b)) => Object::Integer(a - b),
//             (Object::Float(a), Object::Float(b)) => Object::Float(a - b),
//             _ => Object::Null,
//         }
//     }
// }

// impl Rem for Object {
//     type Output = Object;

//     fn rem(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Object::Integer(a), Object::Integer(b)) => Object::Integer(a % b),
//             (Object::Float(a), Object::Float(b)) => Object::Float(a % b),
//             (Object::Null, Object::Null) => Object::Null,
//             _ => Object::Null,
//         }
//     }
// }

// impl Div for Object {
//     type Output = Object;

//     fn div(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Object::Integer(a), Object::Integer(b)) => Object::Integer(a / b),
//             (Object::Float(a), Object::Float(b)) => Object::Float(a / b),
//             (Object::Null, Object::Null) => Object::Null,
//             _ => Object::Null,
//         }
//     }
// }

// impl Mul for Object {
//     type Output = Object;

//     fn mul(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Object::Integer(a), Object::Integer(b)) => Object::Integer(a * b),
//             (Object::Float(a), Object::Float(b)) => Object::Float(a * b),
//             (Object::Null, Object::Null) => Object::Null,
//             _ => Object::Null,
//         }
//     }
// }

// impl BitXor for Object {
//     type Output = Object;

//     fn bitxor(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Object::Integer(a), Object::Integer(b)) => {
//                 Object::Integer(a.pow(b.unsigned_abs() as u32))
//             }
//             (Object::Float(a), Object::Float(b)) => Object::Float(a.powf(b)),
//             (Object::Null, Object::Null) => Object::Null,
//             _ => Object::Null,
//         }
//     }
// }

// impl Display for Object {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Object::Integer(x) => write!(f, "{x}"),
//             Object::Float(x) => write!(f, "{x}"),
//             Object::Bool(x) => write!(f, "{x}"),
//             Object::Str(x) => write!(f, "{x}"),
//             Object::Null => write!(f, ""),
//             Object::Return(_) => unreachable!(),
//             Object::Function {
//                 body,
//                 name,
//                 env,
//                 parameters,
//             } => unreachable!(),
//         }
//     }
// }
