//! Grammar for made-lang.
//!
//! program -> statement* EOF
//!
//! block -> "{" statement* "}"
//!
//! statement -> ( variable_declaration
//!             | typealias_declaration
//!             | named_function_declaration
//!             | while_statement
//!             | for_statement
//!             | return_statement
//!             | break_statement
//!             | continue_statement
//!             | assignment_statement
//!             | struct_declaration
//!             | enum_declaration
//!             | trait_declaration
//!             | impl_declaration
//!             | expression ) ";"?
//!
//! variable_declaration -> "let" IDENTIFIER (":" type)? "=" expression ";"
//!
//! typealias_declaration -> "typealias" IDENTIFIER "=" type ";"
//!
//! named_function_declaration -> "func" IDENTIFIER function_signature block
//! function_signature -> "(" parameter_list ")" ("->" type)?
//!  
//! while_statement -> "while" expression block
//! for_statement -> "for" IDENTIFIER "in" expression block
//! return_statement -> "return" expression? ";"
//! break_statement -> "break" ";"
//! continue_statement -> "continue" ";"
//!
//! struct_declaration -> "struct" IDENTIFIER "{" field_list "}"
//! field_list -> field ( "," field )*
//! field -> IDENTIFIER ":" type
//!
//! enum_declaration -> "enum" IDENTIFIER "{" variant_list "}"
//! variant_list -> variant ( "," variant )*
//! variant -> IDENTIFIER ( "(" type_list ")" | "{" field_list "}" )?
//!
//! trait_declaration -> "trait" IDENTIFIER "{" named_function_declaration* "}"
//!
//! impl_declaration -> "impl" ( trait "for" )? type "{" named_function_declaration* "}"
//! trait -> type
//!
//! assignment_statement -> field_access_expression "=" expression ";"
//!
//! expression -> if_expression
//!             | match_expression
//!             | lambda_declaration
//!             | logical_or_expression
//!
//! logical_or_expression -> logical_and_expression (("or" | "||") logical_and_expression)*
//! logical_and_expression -> equality_expression (("and" | "&&") equality_expression)*
//! equality_expression -> comparison_expression (("==" | "!=") comparison_expression)*
//! comparison_expression -> additive_expression ((">" | "<" | ">=" | "<=") additive_expression)*
//! additive_expression -> multiplicative_expression (("+" | "-") multiplicative_expression)*
//! multiplicative_expression -> unary_expression (("*" | "/" | "%") unary_expression)*
//! unary_expression -> ("+" | "-" | "not") unary_expression | primary_expression
//!
//! primary_expression -> literal
//!                     | IDENTIFIER
//!                     | "(" expression ")"
//!                     | function_call_expression
//!                     | field_access_expression
//!
//! if_expression -> "if" expression block ("else" if_expression | block)?
//! match_expression -> "match" expression "{" match_arm+ "}"
//! match_arm -> (pattern ("|" pattern)*) "=>" block ";"
//!
//! lambda_declaration -> function_signature block
//!
//! field_access_expression -> primary_expression "." IDENTIFIER
//! function_call_expression -> primary_expression "(" argument_list? ")"
//!
//! parameter_list -> parameter ("," parameter)*
//! parameter -> IDENTIFIER ":" type
//! argument_list -> expression ("," expression)*
//!
//! literal -> boolean_literal
//!          | integer_literal
//!          | float_literal
//!          | char_literal
//!          | string_literal
//!
//! boolean_literal -> "true" | "false"
//! integer_literal -> DIGIT (DIGIT | "'")*
//! float_literal -> integer_literal "." integer_literal
//! char_literal -> "'" CHAR "'"
//! string_literal -> "\"" CHAR* "\""
//!
//! type -> IDENTIFIER ( "<" type_list ">" )?
//!
//! type_list -> type ("," type)*
//!
//! DIGIT -> regex("[0-9]")
//! CHAR -> regex("([^\"\\n\\r])")
//! IDENTIFIER -> regex("[\s\'\"]+")
//! WHITESPACE -> regex("\s+") -> skip
//! COMMENT -> regex("#.*") -> skip

use crate::lexer;
use crate::parser::error::ParseError;
use either::Either;
use std::fmt::Display;

pub(crate) type Program = Vec<Statement>;

pub(crate) type Block = Vec<Statement>;

pub(crate) enum Statement {
    VariableDeclaration(VariableDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    WhileLoop(WhileStatement),
    ForLoop(ForStatement),
    Return(ReturnStatement),
    Break,
    Continue,
    Assignment(AssignmentStatement),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    TraitDeclaration(TraitDeclaration),
    ImplDeclaration(ImplDeclaration),
    Expression(Expression),
}

pub(crate) struct VariableDeclaration {
    pub(crate) name: Identifier,
    pub(crate) type_annotation: Option<Type>,
    pub(crate) value: Expression,
}

pub(crate) struct TypeAliasDeclaration {
    pub(crate) name: Identifier,
    pub(crate) type_value: Type,
}

pub(crate) struct FunctionDeclaration {
    pub(crate) name: Identifier,
    pub(crate) signature: FunctionSignature,
    pub(crate) body: Block,
}

pub(crate) struct StructDeclaration {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<Field>,
}

pub(crate) struct EnumDeclaration {
    pub(crate) name: Identifier,
    pub(crate) variants: Vec<Variant>,
}

pub(crate) struct TraitDeclaration {
    pub(crate) name: Identifier,
    pub(crate) functions: Vec<FunctionDeclaration>,
}

pub(crate) struct ImplDeclaration {
    pub(crate) trait_type: Option<Type>,
    pub(crate) for_type: Type,
    pub(crate) functions: Vec<FunctionDeclaration>,
}

pub(crate) struct WhileStatement {
    pub(crate) condition: Expression,
    pub(crate) body: Block,
}

pub(crate) struct ForStatement {
    pub(crate) name: Identifier,
    pub(crate) iterable: Expression,
    pub(crate) body: Block,
}

pub(crate) struct ReturnStatement {
    pub(crate) expression: Option<Expression>,
}

pub(crate) struct AssignmentStatement {
    pub(crate) target: FieldAccessExpression,
    pub(crate) value: Expression,
}
pub(crate) enum Expression {
    If(IfExpression),
    Match(MatchExpression),
    Lambda(LambdaExpression),
    LogicalOr(Box<Expression>, Box<Expression>),
    LogicalAnd(Box<Expression>, Box<Expression>),
    Equality(Box<Expression>, Box<Expression>, EqualityOp),
    Comparison(Box<Expression>, Box<Expression>, ComparisonOp),
    Additive(Box<Expression>, Box<Expression>, AdditiveOp),
    Multiplicative(Box<Expression>, Box<Expression>, MultiplicativeOp),
    Unary(UnaryOp, Box<Expression>),
    Primary(PrimaryExpression),
}

pub(crate) enum EqualityOp {
    Equal,
    NotEqual,
}

pub(crate) enum ComparisonOp {
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

pub(crate) enum AdditiveOp {
    Add,
    Subtract,
}

pub(crate) enum MultiplicativeOp {
    Multiply,
    Divide,
    Modulus,
}

pub(crate) enum UnaryOp {
    Not,
    Negate,
}

pub(crate) enum PrimaryExpression {
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCallExpression),
    FieldAccess(FieldAccessExpression),
}

pub(crate) struct IfExpression {
    pub(crate) condition: Box<Expression>,
    pub(crate) then_block: Block,
    pub(crate) else_block: Option<Box<Either<IfExpression, Block>>>,
}

pub(crate) struct MatchExpression {
    pub(crate) value: Box<Expression>,
    pub(crate) arms: Vec<MatchArm>,
}

pub(crate) struct MatchArm {
    pub(crate) patterns: Vec<Pattern>,
    pub(crate) body: Block,
}

pub(crate) struct LambdaExpression {
    pub(crate) signature: FunctionSignature,
    pub(crate) body: Block,
}

pub(crate) struct FunctionCallExpression {
    pub(crate) function: Box<PrimaryExpression>,
    pub(crate) arguments: Vec<Expression>,
}

pub(crate) struct FieldAccessExpression {
    pub(crate) base: Option<Box<PrimaryExpression>>,
    pub(crate) field: Identifier,
}

pub(crate) enum Literal {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}

pub(crate) struct Type {
    pub(crate) name: Identifier,
    pub(crate) generics: Option<Vec<Type>>,
}

pub(crate) struct Field {
    pub(crate) name: Identifier,
    pub(crate) field_type: Type,
}

pub(crate) struct Variant {
    pub(crate) name: Identifier,
    pub(crate) associated_data: AssociatedData,
}

pub(crate) enum AssociatedData {
    Types(Vec<Type>),
    Fields(Vec<Field>),
    None,
}

pub(crate) struct FunctionSignature {
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) return_type: Option<Type>,
}

pub(crate) struct Parameter {
    pub(crate) name: Identifier,
    pub(crate) parameter_type: Type,
}

pub(crate) enum Pattern {
    Identifier(Identifier),
    Literal(Literal),
    Default,
}

pub(crate) type Identifier = String;

// impls
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Literal::Boolean(b) => format!("Boolean({})", b),
            Literal::Integer(i) => format!("Integer({})", i),
            Literal::Float(f) => format!("Float({})", f),
            Literal::Char(c) => format!("Char('{}')", c),
            Literal::String(s) => format!("String(\"{}\")", s),
        };
        write!(f, "{}", str)
    }
}

impl TryFrom<lexer::Token> for Literal {
    type Error = ParseError;

    fn try_from(value: lexer::Token) -> Result<Self, Self::Error> {
        match value.r#type {
            lexer::TokenType::True => Ok(Literal::Boolean(true)),
            lexer::TokenType::False => Ok(Literal::Boolean(false)),
            lexer::TokenType::Integer(i) => Ok(Literal::Integer(i)),
            lexer::TokenType::Float(f) => Ok(Literal::Float(f)),
            lexer::TokenType::Char(c) => Ok(Literal::Char(c)),
            lexer::TokenType::String(s) => Ok(Literal::String(s)),
            _ => Err(ParseError::UnexpectedToken(value)),
        }
    }
}

