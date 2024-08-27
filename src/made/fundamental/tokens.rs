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
    Backslash, // \
    At,        // @
    Pipe,      // |

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
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
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

    Null, // null

    If,   // if
    Else, // else

    For,      // for
    While,    // while
    Loop,     // loop
    Break,    // break
    Continue, // continue

    Struct, // struct
    Fn,     // fn
    Return, // return

    Let, // let

    Eof,
}
impl TokenType {
    pub fn try_parse_keyword(keyword: &str) -> Option<Self> {
        Some(match keyword {
            "and" => Self::And,
            "or" => Self::Or,
            "true" => Self::True,
            "false" => Self::False,
            "null" => Self::Null,
            "if" => Self::If,
            "else" => Self::Else,
            "for" => Self::For,
            "while" => Self::While,
            "loop" => Self::Loop,
            "break" => Self::Break,
            "continue" => Self::Continue,
            "struct" => Self::Struct,
            "fn" => Self::Fn,
            "return" => Self::Return,
            _ => return None,
        })
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token {
    r#type: TokenType,
    line: usize,
    lexeme: String,
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
