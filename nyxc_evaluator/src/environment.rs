// use std::collections::HashMap;

// use crate::object::Object;

// // #[derive(Debug, Clone)]
// // pub struct Variables {
// //     pub name: String,
// //     pub mutable: bool,
// // }

// #[derive(Debug, PartialEq, Clone)]
// pub struct Environment {
//     store: HashMap<String, Object>,
// }

// impl Default for Environment {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl Environment {
//     pub fn new() -> Self {
//         Self {
//             store: HashMap::new(),
//         }
//     }

//     pub fn set(&mut self, k: &str, v: Object) {
//         if k != "_" {
//             self.store.insert(k.to_owned(), v);
//         }
//     }

//     pub fn get(&self, k: &str) -> Option<Object> {
//         self.store.get(k).cloned()
//     }
// }
