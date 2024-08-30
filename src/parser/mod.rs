mod grammar;

#[macro_export]
macro_rules! define_grammar {
    (
        $grammar_name:ident =>
        $(
            $name:ident {
                $( $field:ident : $type:ty ),* $(,)?
            }
        ),* $(,)?
    ) => {paste::paste! {

        #[derive(Debug)]
        enum $grammar_name {
            $(
                $name { $( $field : $type ),* },
            )*
        }

        impl $grammar_name {
            fn accept<R>(&self, visitor: &mut dyn  Visitor< $grammar_name ,R> ) -> R {
                match self {
                    $(
                        $grammar_name::$name { .. } => visitor.[<visit_$name:snake>](self),
                    )*
                }
            }
        }


        trait Visitor<T, R> {
            $(
                fn [<visit_$name:snake>](&mut self, t: &T) -> R;
            )*
        }

    }};
}
