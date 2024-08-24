use std::fs::read;

pub mod made;

fn main() {
    let source = read("../example.ma").unwrap();
    let source = String::from_utf8(source).unwrap();
}
