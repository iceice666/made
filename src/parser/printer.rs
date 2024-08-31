use super::ast_nodes::*;
use crate::parser::Resolver;
use either::Either;


impl Resolver<String> for Vec<Statement> {
    fn resolve(&mut self) -> String {
        self.iter_mut()
            .map(|stmt| stmt.resolve())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl Resolver<String> for Statement {
    fn resolve(&mut self) -> String {
        match self {
            Statement::VariableDeclaration(variable) => variable.resolve(),
            Statement::TypeAliasDeclaration(type_declaration) => type_declaration.resolve(),
            Statement::FunctionDeclaration(function_declaration) => function_declaration.resolve(),
            Statement::WhileLoop(while_loop) => while_loop.resolve(),
            Statement::ForLoop(for_loop) => for_loop.resolve(),
            Statement::Break => "break;".to_string(),
            Statement::Continue => "continue;".to_string(),
            Statement::Return(return_statement) => return_statement.resolve(),
            Statement::Assignment(assignment) => assignment.resolve(),
            Statement::StructDeclaration(struct_declaration) => struct_declaration.resolve(),
            Statement::EnumDeclaration(enum_declaration) => enum_declaration.resolve(),
            Statement::TraitDeclaration(trait_declaration) => trait_declaration.resolve(),
            Statement::ImplDeclaration(impl_declaration) => impl_declaration.resolve(),
            Statement::Expression(expression) => expression.resolve(),
        }
    }
}

impl Resolver<String> for Expression {
    fn resolve(&mut self) -> String {
        match self {
            Expression::If(if_expr) => if_expr.resolve(),
            Expression::Match(match_expr) => match_expr.resolve(),
            Expression::Lambda(lambda_expr) => lambda_expr.resolve(),
            Expression::LogicalOr(lhs, rhs) => format!("{} || {}", lhs.resolve(), rhs.resolve()),
            Expression::LogicalAnd(lhs, rhs) => format!("{} && {}", lhs.resolve(), rhs.resolve()),
            Expression::Equality(lhs, rhs, op) => {
                format!("{} {} {}", lhs.resolve(), op.resolve(), rhs.resolve())
            }
            Expression::Comparison(lhs, rhs, op) => {
                format!("{} {} {}", lhs.resolve(), op.resolve(), rhs.resolve())
            }
            Expression::Additive(lhs, rhs, op) => {
                format!("{} {} {}", lhs.resolve(), op.resolve(), rhs.resolve())
            }
            Expression::Multiplicative(lhs, rhs, op) => {
                format!("{} {} {}", lhs.resolve(), op.resolve(), rhs.resolve())
            }
            Expression::Unary(op, expr) => format!("{}{}", op.resolve(), expr.resolve()),
            Expression::Primary(primary) => primary.resolve(),
        }
    }
}

impl Resolver<String> for EqualityOp {
    fn resolve(&mut self) -> String {
        match self {
            EqualityOp::Equal => "==".to_string(),
            EqualityOp::NotEqual => "!=".to_string(),
        }
    }
}

impl Resolver<String> for ComparisonOp {
    fn resolve(&mut self) -> String {
        match self {
            ComparisonOp::GreaterThan => ">".to_string(),
            ComparisonOp::LessThan => "<".to_string(),
            ComparisonOp::GreaterThanOrEqual => ">=".to_string(),
            ComparisonOp::LessThanOrEqual => "<=".to_string(),
        }
    }
}

impl Resolver<String> for AdditiveOp {
    fn resolve(&mut self) -> String {
        match self {
            AdditiveOp::Add => "+".to_string(),
            AdditiveOp::Subtract => "-".to_string(),
        }
    }
}

impl Resolver<String> for MultiplicativeOp {
    fn resolve(&mut self) -> String {
        match self {
            MultiplicativeOp::Multiply => "*".to_string(),
            MultiplicativeOp::Divide => "/".to_string(),
            MultiplicativeOp::Modulus => "%".to_string(),
        }
    }
}

impl Resolver<String> for UnaryOp {
    fn resolve(&mut self) -> String {
        match self {
            UnaryOp::Not => "!".to_string(),
            UnaryOp::Negate => "-".to_string(),
        }
    }
}

impl Resolver<String> for PrimaryExpression {
    fn resolve(&mut self) -> String {
        match self {
            PrimaryExpression::Literal(literal) => literal.resolve(),
            PrimaryExpression::Identifier(identifier) => identifier.clone(),
            PrimaryExpression::FunctionCall(function_call) => function_call.resolve(),
            PrimaryExpression::FieldAccess(field_access) => field_access.resolve(),
        }
    }
}

impl Resolver<String> for IfExpression {
    fn resolve(&mut self) -> String {
        let mut result = format!(
            "if ({}) {{ {} }}",
            self.condition.resolve(),
            self.then_block.resolve()
        );
        if let Some(else_block) = &mut self.else_block {
            result.push_str(&format!(" else {}", else_block.resolve()));
        }
        result
    }
}

impl Resolver<String> for Either<IfExpression, Vec<Statement>> {
    fn resolve(&mut self) -> String {
        either::for_both!(self, s => s.resolve())
    }
}

impl Resolver<String> for MatchExpression {
    fn resolve(&mut self) -> String {
        let arms_resolved = self
            .arms
            .iter_mut()
            .map(|arm| arm.resolve())
            .collect::<Vec<String>>()
            .join(" ");
        format!("match ({}) {{ {} }}", self.value.resolve(), arms_resolved)
    }
}

impl Resolver<String> for MatchArm {
    fn resolve(&mut self) -> String {
        let patterns_resolved = self
            .patterns
            .iter_mut()
            .map(|pattern| pattern.resolve())
            .collect::<Vec<String>>()
            .join(" | ");
        format!("{} => {}", patterns_resolved, self.body.resolve())
    }
}

impl Resolver<String> for Pattern {
    fn resolve(&mut self) -> String {
        match self {
            Pattern::Identifier(id) => id.clone(),
            Pattern::Literal(lit) => lit.resolve(),
            Pattern::Default => "_".to_string(),
        }
    }
}

impl Resolver<String> for LambdaExpression {
    fn resolve(&mut self) -> String {
        format!("|{}| {}", self.signature.resolve(), self.body.resolve())
    }
}

impl Resolver<String> for FunctionCallExpression {
    fn resolve(&mut self) -> String {
        let args_resolved = self
            .arguments
            .iter_mut()
            .map(|arg| arg.resolve())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({})", self.function.resolve(), args_resolved)
    }
}

impl Resolver<String> for FieldAccessExpression {
    fn resolve(&mut self) -> String {
        format!("{}.{}", self.base.resolve(), self.field)
    }
}

impl Resolver<String> for ReturnStatement {
    fn resolve(&mut self) -> String {
        match &mut self.expression {
            Some(expr) => format!("return {};", expr.resolve()),
            None => "return;".to_string(),
        }
    }
}

impl Resolver<String> for AssignmentStatement {
    fn resolve(&mut self) -> String {
        format!("{} = {};", self.name, self.value.resolve())
    }
}

impl Resolver<String> for StructDeclaration {
    fn resolve(&mut self) -> String {
        format!("struct {} {{ {} }}", self.name, self.fields.resolve())
    }
}

impl Resolver<String> for EnumDeclaration {
    fn resolve(&mut self) -> String {
        let variants_resolved = self
            .variants
            .iter_mut()
            .map(|variant| variant.resolve())
            .collect::<Vec<String>>()
            .join(", ");
        format!("enum {} {{ {} }}", self.name, variants_resolved)
    }
}

impl Resolver<String> for TraitDeclaration {
    fn resolve(&mut self) -> String {
        let functions_resolved = self
            .functions
            .iter_mut()
            .map(|func| func.resolve())
            .collect::<Vec<String>>()
            .join("\n");
        format!("trait {} {{ {} }}", self.name, functions_resolved)
    }
}

impl Resolver<String> for ImplDeclaration {
    fn resolve(&mut self) -> String {
        let trait_part = if let Some(trait_type) = &mut self.trait_type {
            format!("{} for ", trait_type.resolve())
        } else {
            String::new()
        };
        let functions_resolved = self
            .functions
            .iter_mut()
            .map(|func| func.resolve())
            .collect::<Vec<String>>()
            .join("\n");
        format!(
            "impl {}{} {{ {} }}",
            trait_part,
            self.for_type.resolve(),
            functions_resolved
        )
    }
}

impl Resolver<String> for VariableDeclaration {
    fn resolve(&mut self) -> String {
        let mut declaration = format!("let {}", self.name);
        if let Some(type_annotation) = &mut self.type_annotation {
            declaration.push_str(format!(": {}", type_annotation.resolve()).as_str());
        }

        declaration.push_str(" = ");
        declaration.push_str(&self.value.resolve());

        declaration
    }
}

impl Resolver<String> for TypeAliasDeclaration {
    fn resolve(&mut self) -> String {
        format!("type {} = {}", self.name, self.type_value.resolve())
    }
}

impl Resolver<String> for FunctionDeclaration {
    fn resolve(&mut self) -> String {
        format!(
            "fn {}{} {{ {} }}",
            self.name,
            self.signature.resolve(),
            self.body.resolve()
        )
    }
}

impl Resolver<String> for WhileStatement {
    fn resolve(&mut self) -> String {
        format!(
            "while ({}) {{ {} }}",
            self.condition.resolve(),
            self.body.resolve()
        )
    }
}

impl Resolver<String> for ForStatement {
    fn resolve(&mut self) -> String {
        format!(
            "for {} in {} {{ {} }}",
            self.name,
            self.iterable.resolve(),
            self.body.resolve()
        )
    }
}

impl Resolver<String> for Literal {
    fn resolve(&mut self) -> String {
        format!("{}", self)
    }
}

impl Resolver<String> for Type {
    fn resolve(&mut self) -> String {
        let mut type_ = self.name.clone();
        if let Some(generics) = &mut self.generics {
            type_.push_str(format!("<{}>", generics.resolve()).as_str());
        }
        type_
    }
}

impl Resolver<String> for Vec<Type> {
    fn resolve(&mut self) -> String {
        self.iter_mut()
            .map(|ty| ty.resolve())
            .collect::<Vec<String>>()
            .join(",")
    }
}

impl Resolver<String> for Field {
    fn resolve(&mut self) -> String {
        format!("{}: {}", self.name, self.field_type.resolve())
    }
}

impl Resolver<String> for Vec<Field> {
    fn resolve(&mut self) -> String {
        self.iter_mut()
            .map(|field| field.resolve())
            .collect::<Vec<String>>()
            .join(",")
    }
}

impl Resolver<String> for Parameter {
    fn resolve(&mut self) -> String {
        format!("{}: {}", self.name, self.parameter_type.resolve())
    }
}

impl Resolver<String> for Vec<Parameter> {
    fn resolve(&mut self) -> String {
        self.iter_mut()
            .map(|param| param.resolve())
            .collect::<Vec<String>>()
            .join(",")
    }
}

impl Resolver<String> for Variant {
    fn resolve(&mut self) -> String {
        format!("{}{}", self.name, self.associated_data.resolve())
    }
}

impl Resolver<String> for AssociatedData {
    fn resolve(&mut self) -> String {
        match self {
            AssociatedData::None => "".to_string(),
            AssociatedData::Types(types) => format!("({})", types.resolve()),
            AssociatedData::Fields(fields) => format!("{{{}}}", fields.resolve()),
        }
    }
}

impl Resolver<String> for FunctionSignature {
    fn resolve(&mut self) -> String {
        format!(
            "({}) -> {}",
            self.parameters.resolve(),
            self.return_type.resolve()
        )
    }
}

impl Resolver<String> for Option<Type> {
    fn resolve(&mut self) -> String {
        match self {
            Some(type_) => type_.resolve(),
            None => "()".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn identifier(name: &str) -> Identifier {
        name.to_string()
    }

    #[test]
    fn test_literal_resolver() {
        let mut literal_boolean = Literal::Boolean(true);
        let mut literal_integer = Literal::Integer(42);
        let mut literal_float = Literal::Float(3.14);
        let mut literal_char = Literal::Char('a');
        let mut literal_string = Literal::String("hello".to_string());

        assert_eq!(literal_boolean.resolve(), "Boolean(true)");
        assert_eq!(literal_integer.resolve(), "Integer(42)");
        assert_eq!(literal_float.resolve(), "Float(3.14)");
        assert_eq!(literal_char.resolve(), "Char('a')");
        assert_eq!(literal_string.resolve(), "String(\"hello\")");
    }

    #[test]
    fn test_variable_declaration_resolver() {
        let mut variable = VariableDeclaration {
            name: identifier("x"),
            type_annotation: Some(Type {
                name: identifier("i32"),
                generics: None,
            }),
            value: Expression::Primary(PrimaryExpression::Literal(Literal::Integer(10))),
        };

        assert_eq!(variable.resolve(), "let x: i32 = Integer(10)");
    }

    #[test]
    fn test_function_signature_resolver() {
        let mut signature = FunctionSignature {
            parameters: vec![
                Parameter {
                    name: identifier("a"),
                    parameter_type: Type {
                        name: identifier("i32"),
                        generics: None,
                    },
                },
                Parameter {
                    name: identifier("b"),
                    parameter_type: Type {
                        name: identifier("f64"),
                        generics: None,
                    },
                },
            ],
            return_type: Some(Type {
                name: identifier("i32"),
                generics: None,
            }),
        };

        assert_eq!(signature.resolve(), "(a: i32,b: f64) -> i32");
    }

    #[test]
    fn test_function_declaration_resolver() {
        let mut function = FunctionDeclaration {
            name: identifier("sum"),
            signature: FunctionSignature {
                parameters: vec![
                    Parameter {
                        name: identifier("a"),
                        parameter_type: Type {
                            name: identifier("i32"),
                            generics: None,
                        },
                    },
                    Parameter {
                        name: identifier("b"),
                        parameter_type: Type {
                            name: identifier("i32"),
                            generics: None,
                        },
                    },
                ],
                return_type: Some(Type {
                    name: identifier("i32"),
                    generics: None,
                }),
            },
            body: vec![Statement::Return(ReturnStatement {
                expression: Some(Expression::Additive(
                    Box::new(Expression::Primary(PrimaryExpression::Identifier(
                        identifier("a"),
                    ))),
                    Box::new(Expression::Primary(PrimaryExpression::Identifier(
                        identifier("b"),
                    ))),
                    AdditiveOp::Add,
                )),
            })],
        };

        assert_eq!(
            function.resolve(),
            "fn sum(a: i32,b: i32) -> i32 { return a + b; }"
        );
    }

    #[test]
    fn test_if_expression_resolver() {
        let mut if_expr = IfExpression {
            condition: Box::new(Expression::Equality(
                Box::new(Expression::Primary(PrimaryExpression::Identifier(
                    identifier("x"),
                ))),
                Box::new(Expression::Primary(PrimaryExpression::Literal(
                    Literal::Integer(0),
                ))),
                EqualityOp::Equal,
            )),
            then_block: vec![Statement::Return(ReturnStatement {
                expression: Some(Expression::Primary(PrimaryExpression::Literal(
                    Literal::String("zero".to_string()),
                ))),
            })],
            else_block: Some(Box::new(Either::Left(IfExpression {
                condition: Box::new(Expression::Equality(
                    Box::new(Expression::Primary(PrimaryExpression::Identifier(
                        identifier("x"),
                    ))),
                    Box::new(Expression::Primary(PrimaryExpression::Literal(
                        Literal::Integer(1),
                    ))),
                    EqualityOp::Equal,
                )),
                then_block: vec![Statement::Return(ReturnStatement {
                    expression: Some(Expression::Primary(PrimaryExpression::Literal(
                        Literal::String("one".to_string()),
                    ))),
                })],
                else_block: None,
            }))),
        };

        assert_eq!(
            if_expr.resolve(),
            "if (x == Integer(0)) { return String(\"zero\"); } else if (x == Integer(1)) { return String(\"one\"); }"
        );
    }

    #[test]
    fn test_match_expression_resolver() {
        let mut match_expr = MatchExpression {
            value: Box::new(Expression::Primary(PrimaryExpression::Identifier(
                identifier("x"),
            ))),
            arms: vec![
                MatchArm {
                    patterns: vec![Pattern::Literal(Literal::Integer(1))],
                    body: vec![Statement::Return(ReturnStatement {
                        expression: Some(Expression::Primary(PrimaryExpression::Literal(
                            Literal::String("one".to_string()),
                        ))),
                    })],
                },
                MatchArm {
                    patterns: vec![Pattern::Default],
                    body: vec![Statement::Return(ReturnStatement {
                        expression: Some(Expression::Primary(PrimaryExpression::Literal(
                            Literal::String("other".to_string()),
                        ))),
                    })],
                },
            ],
        };

        assert_eq!(
            match_expr.resolve(),
            "match (x) { Integer(1) => return String(\"one\"); _ => return String(\"other\"); }"
        );
    }

    #[test]
    fn test_struct_declaration_resolver() {
        let mut struct_decl = StructDeclaration {
            name: identifier("Point"),
            fields: vec![
                Field {
                    name: identifier("x"),
                    field_type: Type {
                        name: identifier("f64"),
                        generics: None,
                    },
                },
                Field {
                    name: identifier("y"),
                    field_type: Type {
                        name: identifier("f64"),
                        generics: None,
                    },
                },
            ],
        };

        assert_eq!(struct_decl.resolve(), "struct Point { x: f64,y: f64 }");
    }

    #[test]
    fn test_enum_declaration_resolver() {
        let mut enum_decl = EnumDeclaration {
            name: identifier("Result"),
            variants: vec![
                Variant {
                    name: identifier("Ok"),
                    associated_data: AssociatedData::Types(vec![Type {
                        name: identifier("T"),
                        generics: None,
                    }]),
                },
                Variant {
                    name: identifier("Err"),
                    associated_data: AssociatedData::Types(vec![Type {
                        name: identifier("E"),
                        generics: None,
                    }]),
                },
            ],
        };

        assert_eq!(enum_decl.resolve(), "enum Result { Ok(T), Err(E) }");
    }

    #[test]
    fn test_trait_declaration_resolver() {
        let mut trait_decl = TraitDeclaration {
            name: identifier("Display"),
            functions: vec![FunctionDeclaration {
                name: identifier("fmt"),
                signature: FunctionSignature {
                    parameters: vec![
                        Parameter {
                            name: identifier("self"),
                            parameter_type: Type {
                                name: identifier("&self"),
                                generics: None,
                            },
                        },
                        Parameter {
                            name: identifier("f"),
                            parameter_type: Type {
                                name: identifier("&mut Formatter"),
                                generics: None,
                            },
                        },
                    ],
                    return_type: Some(Type {
                        name: identifier("Result"),
                        generics: Some(vec![
                            Type {
                                name: identifier("()"),
                                generics: None,
                            },
                            Type {
                                name: identifier("Error"),
                                generics: None,
                            },
                        ]),
                    }),
                },
                body: vec![],
            }],
        };

        assert_eq!(
            trait_decl.resolve(),
            "trait Display { fn fmt(self: &self,f: &mut Formatter) -> Result<(),Error> {  } }"
        );
    }

    #[test]
    fn test_impl_declaration_resolver() {
        let mut impl_decl = ImplDeclaration {
            trait_type: Some(Type {
                name: identifier("Display"),
                generics: None,
            }),
            for_type: Type {
                name: identifier("Point"),
                generics: None,
            },
            functions: vec![FunctionDeclaration {
                name: identifier("fmt"),
                signature: FunctionSignature {
                    parameters: vec![
                        Parameter {
                            name: identifier("self"),
                            parameter_type: Type {
                                name: identifier("&self"),
                                generics: None,
                            },
                        },
                        Parameter {
                            name: identifier("f"),
                            parameter_type: Type {
                                name: identifier("&mut Formatter"),
                                generics: None,
                            },
                        },
                    ],
                    return_type: Some(Type {
                        name: identifier("Result"),
                        generics: Some(vec![
                            Type {
                                name: identifier("()"),
                                generics: None,
                            },
                            Type {
                                name: identifier("Error"),
                                generics: None,
                            },
                        ]),
                    }),
                },
                body: vec![],
            }],
        };

        assert_eq!(
            impl_decl.resolve(),
            "impl Display for Point { fn fmt(self: &self,f: &mut Formatter) -> Result<(),Error> {  } }"
        );
    }
}
