use std::fs::read;


mod lexer;
mod parser;

fn main() {
    let source = read("../example.ma").unwrap();
    let source = String::from_utf8(source).unwrap();
}
