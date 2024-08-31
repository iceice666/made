pub(crate) mod nodes;
mod printer;
mod source;

pub(crate) trait Resolver<T> {
    fn resolve(&mut self) -> T;
}

