pub(crate) mod ast_nodes;
mod printer;
mod source;
mod parser;


pub(crate) trait Resolver<T> {
    fn resolve(&mut self) -> T;
}

