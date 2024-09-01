pub(crate) mod ast_nodes;
mod error;
mod parser;
mod printer;
mod source;

pub(crate) trait Resolver<T> {
    fn resolve(&mut self) -> T;
}
