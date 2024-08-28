use crate::lexer::tokens::Token;

// grammar
//
//  True -> "true"
//  False -> "false"
//  Not -> "not"
//
//  Boolean -> True | False
//  Literal -> Integer | Float | String | Boolean
//  Unary -> ( Not | "-" ) Expression
//  Binary -> Expression Operator Expression
//  Grouping -> "(" Expression ")"
//  Variable -> Identifier
//  Assignment -> Identifier "=" Expression
//  Call -> Expression "(" [Expression ("," Expression)*] ")"
//  Get -> Expression "." Identifier
//  Set -> Expression "." Identifier "=" Expression
//  Let -> "let" Identifier (":" Type)? "=" Expression
//  Expression -> Literal | Variable | Assignment | Unary | Binary | Grouping | Call | Get | Set | Let

macro_rules! define_expr {
    (
        $(
            $name:ident {
                $( $field:ident : $type:ty ),* $(,)?
            }
        ),* $(,)?
    ) => {  paste::paste!  {
        #[derive(Debug)]
        enum Expr {
            $(
                $name { $( $field : $type ),* },
            )*
        }

        impl Expr {
            fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
                match self {
                    $(
                        Expr::$name { .. } => visitor.[<visit_$name:snake>](self),
                    )*
                }
            }
        }

        trait Visitor<R> {
            $(
                fn [<visit_$name:snake>](&mut self, expr: &Expr) -> R;
            )*
        }

   } };
}

define_expr! {
    Boolean { value: bool },
    Integer { value: i64 },
    Float { value: f64 },
    String { value: String },
    Unary { operator: Token, right: Box<Expr> },
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Grouping { expression: Box<Expr> },
    Variable { name: String },
    Assignment { name: String, value: Box<Expr> },
    Call { callee: Box<Expr>, arguments: Vec<Expr> },
    Get { object: Box<Expr>, name: String },
    Set { object: Box<Expr>, name: String, value: Box<Expr> },
    Let { name: String, value: Box<Expr>, ty: Option<String> }
}

struct AstPrinter;

impl Visitor<String> for AstPrinter {
    fn visit_boolean(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Boolean { value } => {
                if *value {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_integer(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Integer { value } => value.to_string(),
            _ => unreachable!(),
        }
    }

    fn visit_float(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Float { value } => value.to_string(),
            _ => unreachable!(),
        }
    }

    fn visit_string(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::String { value } => value.clone(),
            _ => unreachable!(),
        }
    }

    fn visit_unary(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Unary { operator, right } => {
                format!("({} {})", operator.r#type, right.accept(self))
            }
            _ => unreachable!(),
        }
    }

    fn visit_binary(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                format!(
                    "({} {} {})",
                    left.accept(self),
                    operator.r#type,
                    right.accept(self)
                )
            }
            _ => unreachable!(),
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Grouping { expression } => {
                format!("({})", expression.accept(self))
            }
            _ => unreachable!(),
        }
    }

    fn visit_variable(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Variable { name } => name.clone(),
            _ => unreachable!(),
        }
    }

    fn visit_assignment(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Assignment { name, value } => {
                format!("({} = {})", name, value.accept(self))
            }
            _ => unreachable!(),
        }
    }

    fn visit_call(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Call { callee, arguments } => {
                let mut args = String::new();
                for arg in arguments {
                    args.push_str(&arg.accept(self));
                    args.push_str(", ");
                }
                format!("({} {})", callee.accept(self), args)
            }
            _ => unreachable!(),
        }
    }

    fn visit_get(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Get { object, name } => {
                format!("({}.{})", object.accept(self), name)
            }
            _ => unreachable!(),
        }
    }

    fn visit_set(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Set {
                object,
                name,
                value,
            } => {
                format!(
                    "({}.{} = {})",
                    object.accept(self),
                    name,
                    value.accept(self)
                )
            }
            _ => unreachable!(),
        }
    }

    fn visit_let(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Let { name, value, ty } => {
                let ty_str = if let Some(ty) = ty {
                    format!(": {}", ty)
                } else {
                    "".to_string()
                };
                format!("let {}{} = {}", name, ty_str, value.accept(self))
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokens::Token;
    use crate::lexer::tokens::TokenType::*;

    #[test]
    fn test_boolean_expression() {
        let expr = Expr::Boolean { value: true };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_boolean(&expr), "true".to_string());

        let expr = Expr::Boolean { value: false };
        assert_eq!(printer.visit_boolean(&expr), "false".to_string());
    }

    #[test]
    fn test_integer_expression() {
        let expr = Expr::Integer { value: 42 };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_integer(&expr), "42".to_string());

        let expr = Expr::Integer { value: i64::MIN };
        assert_eq!(printer.visit_integer(&expr), i64::MIN.to_string());
        let expr = Expr::Integer { value: i64::MAX };
        assert_eq!(printer.visit_integer(&expr), i64::MAX.to_string());
    }

    #[test]
    fn test_float_expression() {
        #[allow(clippy::approx_constant)]
        let expr = Expr::Float { value: 3.14 };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_float(&expr), "3.14".to_string());

        let expr = Expr::Float { value: f64::MIN };
        assert_eq!(printer.visit_float(&expr), f64::MIN.to_string());
        let expr = Expr::Float { value: f64::MAX };
        assert_eq!(printer.visit_float(&expr), f64::MAX.to_string());
    }

    #[test]
    fn test_string_expression() {
        let expr = Expr::String {
            value: "hello".to_string(),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_string(&expr), "hello".to_string());

        let expr = Expr::String {
            value: "".to_string(),
        };
        assert_eq!(printer.visit_string(&expr), "".to_string());
    }

    #[test]
    fn test_unary_expression() {
        let expr = Expr::Unary {
            operator: Token::new_with_type(Minus),
            right: Box::new(Expr::Integer { value: 5 }),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_unary(&expr), "(- 5)".to_string());

        let expr = Expr::Unary {
            operator: Token::new_with_type(Not),
            right: Box::new(Expr::Boolean { value: true }),
        };
        assert_eq!(printer.visit_unary(&expr), "(not true)".to_string());
    }

    #[test]
    fn test_binary_expression() {
        let expr = Expr::Binary {
            left: Box::new(Expr::Integer { value: 1 }),
            operator: Token::new_with_type(Plus),
            right: Box::new(Expr::Integer { value: 2 }),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_binary(&expr), "(1 + 2)".to_string());

        let expr = Expr::Binary {
            left: Box::new(Expr::Float { value: 1.0 }),
            operator: Token::new_with_type(Minus),
            right: Box::new(Expr::Float { value: 2.0 }),
        };
        assert_eq!(printer.visit_binary(&expr), "(1 - 2)".to_string());
    }

    #[test]
    fn test_grouping_expression() {
        let expr = Expr::Grouping {
            expression: Box::new(Expr::Integer { value: 1 }),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_grouping(&expr), "(1)".to_string());

        let expr = Expr::Grouping {
            expression: Box::new(Expr::Binary {
                left: Box::new(Expr::Integer { value: 1 }),
                operator: Token::new_with_type(Plus),
                right: Box::new(Expr::Integer { value: 2 }),
            }),
        };
        assert_eq!(printer.visit_grouping(&expr), "((1 + 2))".to_string());
    }

    #[test]
    fn test_variable_expression() {
        let expr = Expr::Variable {
            name: "x".to_string(),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_variable(&expr), "x".to_string());

        let expr = Expr::Variable {
            name: "".to_string(),
        };
        assert_eq!(printer.visit_variable(&expr), "".to_string());
    }

    #[test]
    fn test_assignment_expression() {
        let expr = Expr::Assignment {
            name: "x".to_string(),
            value: Box::new(Expr::Integer { value: 1 }),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_assignment(&expr), "(x = 1)".to_string());

        let expr = Expr::Assignment {
            name: "".to_string(),
            value: Box::new(Expr::Integer { value: 1 }),
        };
        assert_eq!(printer.visit_assignment(&expr), "( = 1)".to_string());
    }

    #[test]
    fn test_call_expression() {
        let expr = Expr::Call {
            callee: Box::new(Expr::Variable {
                name: "f".to_string(),
            }),
            arguments: vec![Expr::Integer { value: 1 }],
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_call(&expr), "(f 1, )".to_string());

        let expr = Expr::Call {
            callee: Box::new(Expr::Variable {
                name: "f".to_string(),
            }),
            arguments: vec![],
        };
        assert_eq!(printer.visit_call(&expr), "(f )".to_string());
    }

    #[test]
    fn test_get_expression() {
        let expr = Expr::Get {
            object: Box::new(Expr::Variable {
                name: "obj".to_string(),
            }),
            name: "prop".to_string(),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_get(&expr), "(obj.prop)".to_string());

        let expr = Expr::Get {
            object: Box::new(Expr::Variable {
                name: "".to_string(),
            }),
            name: "".to_string(),
        };
        assert_eq!(printer.visit_get(&expr), "(.)".to_string());
    }

    #[test]
    fn test_set_expression() {
        let expr = Expr::Set {
            object: Box::new(Expr::Variable {
                name: "obj".to_string(),
            }),
            name: "prop".to_string(),
            value: Box::new(Expr::Integer { value: 1 }),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_set(&expr), "(obj.prop = 1)".to_string());

        let expr = Expr::Set {
            object: Box::new(Expr::Variable {
                name: "".to_string(),
            }),
            name: "".to_string(),
            value: Box::new(Expr::Integer { value: 1 }),
        };
        assert_eq!(printer.visit_set(&expr), "(. = 1)".to_string());
    }

    #[test]
    fn test_let_expression() {
        let expr = Expr::Let {
            name: "x".to_string(),
            value: Box::new(Expr::Integer { value: 1 }),
            ty: Some("i32".to_string()),
        };
        let mut printer = AstPrinter;
        assert_eq!(printer.visit_let(&expr), "let x: i32 = 1".to_string());

        let expr = Expr::Let {
            name: "".to_string(),
            value: Box::new(Expr::Integer { value: 1 }),
            ty: None,
        };
        assert_eq!(printer.visit_let(&expr), "let  = 1".to_string());
    }
}
