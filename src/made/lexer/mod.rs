mod error;

use crate::made::lexer::error::LexResult;
use crate::made::lexer::error::LexerError;
use crate::made::utils::source::SourceHolder;
use std::convert::Into;

use crate::made::fundamental::{Token, TokenType};

fn add_token(tt: TokenType, source: &SourceHolder, tokens: &mut Vec<Token>) {
    let line = source.line;
    let lexeme = source.get_lexeme().to_string();
    let token = Token::new(tt, line, lexeme);
    tokens.push(token);
}

pub(crate) fn scan_tokens(source: impl Into<String>) -> LexResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut source = SourceHolder::new(source.into());

    macro_rules! add_token {
        ($tt:expr) => {
            add_token($tt, &source, &mut tokens)
        };

        (
            $(
                $expected:literal => $value:expr
            ),+
            , _ => $default:expr  $(,)?

        ) => {
            match source.peek() {
                $(
                    Some($expected) => {
                        add_token($value, &source, &mut tokens);
                        source.advance();  // Advance the iter
                    },
                )*

                _ => add_token($default, &source, &mut tokens)
            }
        }
    }

    while let Some(ch) = source.advance() {
        source.reset_start();

        match ch {
            '(' => add_token!(TokenType::LeftParen),
            ')' => add_token!(TokenType::RightParen),
            '{' => add_token!(TokenType::LeftBrace),
            '}' => add_token!(TokenType::RightBrace),
            ',' => add_token!(TokenType::Comma),
            '.' => add_token!(TokenType::Dot),
            '+' => add_token!(TokenType::Plus),
            '-' => add_token!(TokenType::Minus),
            ';' => add_token!(TokenType::Semicolon),
            '*' => add_token!(TokenType::Star),
            '/' => add_token!(TokenType::Slash),
            '@' => add_token!(TokenType::At),
            '|' => add_token!(TokenType::Pipe),

            '=' => add_token! {
                '=' => TokenType::EqualEqual,
                '>' => TokenType::EqualGreater,
                _ => TokenType::Equal,
            },
            '!' => add_token! {
                '=' => TokenType::BangEqual,
                _ => TokenType::Bang
            },
            '<' => add_token! {
                '=' => TokenType::LessEqual,
                _ => TokenType::Less
            },
            '>' => add_token! {
                '=' => TokenType::GreaterEqual,
                _ => TokenType::Greater
            },
            '#' => {
                while source.peek() != Some('\n') && source.peek().is_some() {
                    source.advance();
                }
                add_token!(TokenType::Comment(source.get_lexeme().to_string()))
            }

            '\'' => match source.advance() {
                Some(first_ch) => {
                    let ch = match first_ch {
                        '\\' => match source.advance() {
                            Some('n') => '\n',
                            Some('t') => '\t',
                            Some('r') => '\r',
                            Some('0') => '\0',
                            Some(escaped_ch) => escaped_ch,
                            None => {
                                return Err(LexerError::UndeterminedCharLiteral(source.to_owned()))
                            }
                        },
                        '\'' => return Err(LexerError::EmptyCharDeclaration(source.to_owned())),
                        _ => first_ch,
                    };

                    add_token!(TokenType::Char(ch));

                    if Some('\'') != source.advance() {
                        return Err(LexerError::UndeterminedCharLiteral(source.to_owned()));
                    }
                }
                None => return Err(LexerError::UndeterminedCharLiteral(source.to_owned())),
            },

            '"' => {
                let token = parse_string(&mut source)?;
                add_token!(token);
                source.advance(); // Closing "
            }

            ch if ch.is_ascii_digit() => {
                let token = try_parse_number(&mut source, ch.to_string()).unwrap();
                add_token!(token);
            }

            ch if ch.is_whitespace() => {}

            _ => {
                // Try parse identifer and keywords
                let mut ident = ch.to_string();
                while let Some(ch) = source.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        source.advance();
                    } else {
                        break;
                    }
                }
                ident.push_str(source.get_lexeme());
                match TokenType::try_parse_keyword(&ident) {
                    Some(tt) => add_token!(tt),
                    None => add_token!(TokenType::Identifier(ident)),
                }

                // return Err(ScannerError::UnexpectedCharacter(source, ch));
            }
        }
    }

    Ok(tokens)
}

fn parse_string(source: &mut SourceHolder) -> LexResult<TokenType> {
    while source.peek() != Some('"') {
        if source.peek().is_none() {
            return Err(LexerError::UndeterminedStringLiteral(source.to_owned()));
        }
        source.advance();
    }
    let value = source.get_lexeme().to_string();
    Ok(TokenType::String(value))
}

fn try_parse_number(source: &mut SourceHolder, mut num_str: String) -> Option<TokenType> {
    let mut float_flag = false;
    while let Some(ch) = source.peek() {
        if ch.is_ascii_digit() || ch == '\'' {
            source.advance();
        } else if !float_flag && ch == '.' {
            source.advance();
            float_flag = true;
        } else {
            break;
        }
    }

    num_str.push_str(source.get_lexeme());
    let num_str = num_str.replace("'", "");
    if num_str.is_empty() {
        None
    } else if float_flag {
        Some(TokenType::Float(num_str.parse().unwrap()))
    } else {
        Some(TokenType::Integer(num_str.parse().unwrap()))
    }
}

#[test]
fn test_ident() {
    todo!();
}

#[test]
fn test_punctuation() -> anyhow::Result<()> {
    let input = ">= <= != > -=";
    let tokens = scan_tokens(input)?;

    println!("{:#?}", tokens);

    Ok(())
}

#[test]
fn test_string() {
    assert_eq!(
        scan_tokens("\"114514\"").unwrap(),
        [Token::new(
            TokenType::String("114514".to_string()),
            1,
            "114514"
        )]
    )
}

#[test]
fn test_number() {
    use TokenType::*;

    assert_eq!(
        scan_tokens("114 -514 19.19 -8.10 114'514'191'981'012'233").unwrap(),
        [
            Token::new(Integer(114,), 1, "14"),
            Token::new(Minus, 1, ""),
            Token::new(Integer(514,), 1, "14"),
            Token::new(Float(19.19,), 1, "9.19"),
            Token::new(Minus, 1, ""),
            Token::new(Float(8.1,), 1, ".10"),
            Token::new(Integer(114514191981012233,), 1, "14'514'191'981'012'233"),
        ]
    )
}
