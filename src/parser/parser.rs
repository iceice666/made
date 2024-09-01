use crate::lexer::{Token, TokenType};
use crate::parser::ast_nodes::*;
use crate::parser::error::{ParseError, ParseResult};
use crate::parser::source::Source;
use either::Either;

pub fn parse(source: Vec<Token>) -> ParseResult<Program> {
    let mut source = Source::new(source);
    Program::parse(&mut source)
}

trait Parser<T> {
    fn parse(source: &mut Source) -> ParseResult<T>;
}

impl Parser<Vec<Statement>> for Vec<Statement> {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        let mut statements = vec![Statement::parse(source)?];

        if source.has_next() {
            let rest = Vec::parse(source)?;
            statements.extend(rest);
        }

        Ok(statements)
    }
}

impl Parser<Statement> for Statement {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        macro_rules! cleanup {
            ($code:expr) => {{
                source.consume()?;
                $code
            }};
        }

        // If u reach eof why u call me? (?_?)"'
        let token = source.peek().ok_or(ParseError::UnexpectedEOF)?;

        let statement = match token.r#type {
            TokenType::Let => cleanup!(Statement::VariableDeclaration(VariableDeclaration::parse(
                source
            )?)),
            TokenType::TypeAlias => cleanup!(Statement::TypeAliasDeclaration(
                TypeAliasDeclaration::parse(source)?
            )),
            TokenType::Func => cleanup!(Statement::FunctionDeclaration(
                FunctionDeclaration::parse(source)?
            )),
            TokenType::While => cleanup!(Statement::WhileLoop(WhileStatement::parse(source)?)),
            TokenType::For => cleanup!(Statement::ForLoop(ForStatement::parse(source)?)),
            TokenType::Return => cleanup!(Statement::Return(ReturnStatement::parse(source)?)),
            TokenType::Break => cleanup!(Statement::Break),
            TokenType::Continue => cleanup!(Statement::Continue),
            TokenType::Struct => cleanup!(Statement::StructDeclaration(StructDeclaration::parse(
                source
            )?)),
            TokenType::Enum => {
                cleanup!(Statement::EnumDeclaration(EnumDeclaration::parse(source)?))
            }
            TokenType::Trait => cleanup!(Statement::TraitDeclaration(TraitDeclaration::parse(
                source
            )?)),
            TokenType::Impl => {
                cleanup!(Statement::ImplDeclaration(ImplDeclaration::parse(source)?))
            }
            _ => {
                // Get an expression statement
                let first_expr = Expression::parse(source)?;

                // Check for assignment statement
                if source.check(TokenType::Equal) {
                    match first_expr {
                        Expression::Primary(PrimaryExpression::FieldAccess(target)) => {
                            cleanup!(Statement::Assignment(AssignmentStatement {
                                target,
                                value: Expression::parse(source)?,
                            }))
                        }
                        Expression::Primary(PrimaryExpression::Identifier(ident)) => {
                            cleanup!(Statement::Assignment(AssignmentStatement {
                                target: FieldAccessExpression {
                                    base: None,
                                    field: ident,
                                },
                                value: Expression::parse(source)?,
                            }))
                        }
                        _ => Statement::Expression(first_expr),
                    }
                } else {
                    Statement::Expression(first_expr)
                }
            }
        };

        Ok(statement)
    }
}

impl Parser<VariableDeclaration> for VariableDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<TypeAliasDeclaration> for TypeAliasDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<FunctionDeclaration> for FunctionDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<WhileStatement> for WhileStatement {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<ForStatement> for ForStatement {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<ReturnStatement> for ReturnStatement {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<StructDeclaration> for StructDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<EnumDeclaration> for EnumDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<TraitDeclaration> for TraitDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<ImplDeclaration> for ImplDeclaration {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<Expression> for Expression {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}

impl Parser<AssignmentStatement> for AssignmentStatement {
    fn parse(source: &mut Source) -> ParseResult<Self> {
        todo!()
    }
}
