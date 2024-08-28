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


    If,   // if
    Else, // else

    For,      // for
    While,    // while
    Loop,     // loop
    Break,    // break
    Continue, // continue

    Struct, // struct
    Impl,   // impl
    Enum,   // enum
    TypeAlias,   // typealias
    Fn,     // fn
    Return, // return

    Let, // let
}


#[derive(Debug, PartialEq)]
pub(crate) struct Token {
    pub(crate)  r#type: TokenType,
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
}
