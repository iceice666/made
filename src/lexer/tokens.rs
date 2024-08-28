use std::fmt;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub(crate) enum TokenType {
    // Punctuation
    LeftParen,   // (
    RightParen,  // )
    LeftBrace,   // {
    RightBrace,  // }
    LeftSquare,  // [
    RightSquare, // ]
    LeftAngle,   // <
    RightAngle,  // >

    Comma,     // ,
    Dot,       // .
    Colon,     // :
    Semicolon, // ;
    BackSlash, // \
    At,        // @
    Pipe,      // |
    Question,  // ?

    MinusGreater, // ->
    EqualGreater, // =>

    // Operands
    Minus,        // -
    Plus,         // +
    Slash,        // /
    Star,         // *
    Bang,         // !
    BangEqual,    // !=
    Equal,        // =
    EqualEqual,   // ==
    GreaterEqual, // >=
    LessEqual,    // <=

    // Literals.
    Identifier(String),
    String(String),
    Char(char),
    Integer(i64),
    Float(f64),
    Comment(String),

    // Keywords.
    And, // and
    Or,  // or

    True,  // true
    False, // false
    Not,   // not

    If,   // if
    Else, // else

    For,      // for
    While,    // while
    Loop,     // loop
    Break,    // break
    Continue, // continue

    Struct,    // struct
    Impl,      // impl
    Enum,      // enum
    TypeAlias, // typealias
    Fn,        // fn
    Return,    // return

    Let, // let
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftSquare => write!(f, "["),
            TokenType::RightSquare => write!(f, "]"),
            TokenType::LeftAngle => write!(f, "<"),
            TokenType::RightAngle => write!(f, ">"),

            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Colon => write!(f, ":"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::BackSlash => write!(f, "\\"),
            TokenType::At => write!(f, "@"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::Question => write!(f, "?"),

            TokenType::MinusGreater => write!(f, "->"),
            TokenType::EqualGreater => write!(f, "=>"),

            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::LessEqual => write!(f, "<="),

            TokenType::Identifier(s) => write!(f, "{}", s),
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::Char(c) => write!(f, "{}", c),
            TokenType::Integer(i) => write!(f, "{}", i),
            TokenType::Float(fl) => write!(f, "{}", fl),
            TokenType::Comment(s) => write!(f, "{}", s),

            TokenType::And => write!(f, "and"),
            TokenType::Or => write!(f, "or"),

            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Not => write!(f, "not"),

            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),

            TokenType::For => write!(f, "for"),
            TokenType::While => write!(f, "while"),
            TokenType::Loop => write!(f, "loop"),
            TokenType::Break => write!(f, "break"),
            TokenType::Continue => write!(f, "continue"),

            TokenType::Struct => write!(f, "struct"),
            TokenType::Impl => write!(f, "impl"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::TypeAlias => write!(f, "typealias"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Return => write!(f, "return"),

            TokenType::Let => write!(f, "let"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) line: usize,
    pub(crate) lexeme: String,
}

impl Token {
    pub fn new(r#type: TokenType, line: usize, lexeme: impl Into<String>) -> Self {
        Self {
            r#type,
            line,
            lexeme: lexeme.into(),
        }
    }

    pub fn new_with_type(r#type: TokenType) -> Self {
        Self {
            r#type,
            line: 0,
            lexeme: String::new(),
        }
    }
}
