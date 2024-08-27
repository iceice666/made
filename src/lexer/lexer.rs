use super::{
    error::{LexResult, LexicalError},
    source::SourceHolder,
    tokens::{Token, TokenType},
};

fn generate_token(tt: TokenType, source: &SourceHolder) -> Token {
    let line = source.line;
    let lexeme = source.get_lexeme().to_string();
    Token::new(tt, line, lexeme)
}

fn scan_token(source: &mut SourceHolder) -> LexResult<Token> {
    macro_rules! new_token {
        ($tt:expr) => {
            generate_token($tt, & source)
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
                        source.advance(); // Advance the iter
                        let token = generate_token($value, & source);
                        token
                    },
                )*
                _ => generate_token($default, & source)
            }
        }
    }

    if let Some(ch) = source.advance() {
        let token = match ch {
            '(' => new_token!(TokenType::LeftParen),
            ')' => new_token!(TokenType::RightParen),
            '{' => new_token!(TokenType::LeftBrace),
            '}' => new_token!(TokenType::RightBrace),
            '[' => new_token!(TokenType::LeftSquare),
            ']' => new_token!(TokenType::RightSquare),
            '<' => new_token! {
                '=' => TokenType::LessEqual,
                _ => TokenType::LeftAngle
            },
            '>' => new_token! {
                '=' => TokenType::GreaterEqual,
                _ => TokenType::RightAngle
            },

            ':' => new_token!(TokenType::Colon),
            '?' => new_token!(TokenType::Question),
            ',' => new_token!(TokenType::Comma),
            '.' => new_token!(TokenType::Dot),
            '+' => new_token!(TokenType::Plus),
            '-' => new_token!(TokenType::Minus),
            ';' => new_token!(TokenType::Semicolon),
            '*' => new_token!(TokenType::Star),
            '/' => new_token!(TokenType::Slash),
            '@' => new_token!(TokenType::At),
            '|' => new_token!(TokenType::Pipe),

            '=' => new_token! {
                '=' => TokenType::EqualEqual,
                '>' => TokenType::EqualGreater,
                _ => TokenType::Equal,
            },
            '!' => new_token! {
                '=' => TokenType::BangEqual,
                _ => TokenType::Bang
            },

            '#' => {
                while source.peek() != Some('\n') && source.peek().is_some() {
                    source.advance();
                }
                let comment = source.get_lexeme().trim_start_matches('#').to_string();
                new_token!(TokenType::Comment(comment))
            }

            '\'' => {
                let tt = try_parse_escaped_char(source)?;

                new_token!(tt)
            }

            '"' => {
                let tt = try_parse_string(source)?;
                source.advance(); // Closing "
                new_token!(tt)
            }

            ch if ch.is_ascii_digit() => {
                let tt = try_parse_number(source)?;
                new_token!(tt)
            }

            ch if ch.is_whitespace() => return Err(LexicalError::Nop),

            _ => {
                while let Some(ch) = source.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        source.advance();
                    } else {
                        break;
                    }
                }

                let ident = source.get_lexeme();
                match try_parse_keyword(ident) {
                    Some(tt) => new_token!(tt),
                    None => new_token!(TokenType::Identifier(ident.to_string())),
                }
            }
        };
        Ok(token)
    } else {
        Err(LexicalError::Eof(source.to_owned()))
    }
}

pub(crate) fn tokenize(source: impl Into<String>) -> LexResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut source = SourceHolder::new(source.into());

    while source.has_next() {
        source.reset_start();
        let token = scan_token(&mut source);
        match token {
            Ok(token) => tokens.push(token),
            Err(LexicalError::Nop) => continue,
            Err(e) => return Err(e),
        }
    }

    Ok(tokens)
}

pub fn try_parse_keyword(keyword: &str) -> Option<TokenType> {
    Some(match keyword {
        "and" => TokenType::And,
        "or" => TokenType::Or,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "null" => TokenType::Null,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "for" => TokenType::For,
        "while" => TokenType::While,
        "loop" => TokenType::Loop,
        "break" => TokenType::Break,
        "continue" => TokenType::Continue,
        "struct" => TokenType::Struct,
        "fn" => TokenType::Fn,
        "return" => TokenType::Return,
        _ => return None,
    })
}

fn try_parse_escaped_char(source: &mut SourceHolder) -> LexResult<TokenType> {
    let res = match source.advance() {
        Some(first_ch) => {
            let ch = match first_ch {
                '\\' => match source.advance() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('r') => '\r',
                    Some('0') => '\0',
                    Some(escaped_ch) => escaped_ch,
                    None => return Err(LexicalError::UndeterminedCharLiteral(source.to_owned())),
                },
                '\'' => return Err(LexicalError::EmptyCharDeclaration(source.to_owned())),
                _ => first_ch,
            };

            Ok(TokenType::Char(ch))
        }
        None => Err(LexicalError::UndeterminedCharLiteral(source.to_owned())),
    };

    // Checking if the closing '
    match source.advance() {
        Some('\'') => res,
        _ => Err(LexicalError::UndeterminedCharLiteral(source.to_owned())),
    }
}


fn try_parse_string(source: &mut SourceHolder) -> LexResult<TokenType> {
    while source.peek() != Some('"') {
        if source.peek().is_none() {
            return Err(LexicalError::UndeterminedStringLiteral(source.to_owned()));
        }
        source.advance();
    }
    let value = source.get_lexeme();
    let value = value.trim_start_matches('"').trim_end_matches('"');
    Ok(TokenType::String(value.to_string()))
}

fn try_parse_number(source: &mut SourceHolder) -> LexResult<TokenType> {
    let mut float_flag = false;
    while let Some(ch) = source.peek() {
        if ch.is_ascii_digit() || ch == '\'' {
            source.advance();
        } else if !float_flag && ch == '.' {
            source.advance();
            float_flag = true;
        } else if float_flag && ch == '.' {
            return Err(LexicalError::MalformedNumber(source.to_owned()));
        } else {
            break;
        }
    }
    let num_str = source.get_lexeme().replace("'", "");
    if num_str.is_empty() {
        Err(LexicalError::HowDidYouGetHere(source.to_owned()))
    } else if float_flag {
        Ok(TokenType::Float(num_str.parse().unwrap()))
    } else {
        Ok(TokenType::Integer(num_str.parse().unwrap()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_punctuation() {
        // 测试正常路径
        let source = "()[]{}<>,.+-;*/@|?";
        let tokens = tokenize(source).unwrap();
        let expected_tokens = vec![
            Token::new(TokenType::LeftParen, 1, "("),
            Token::new(TokenType::RightParen, 1, ")"),
            Token::new(TokenType::LeftSquare, 1, "["),
            Token::new(TokenType::RightSquare, 1, "]"),
            Token::new(TokenType::LeftBrace, 1, "{"),
            Token::new(TokenType::RightBrace, 1, "}"),
            Token::new(TokenType::LeftAngle, 1, "<"),
            Token::new(TokenType::RightAngle, 1, ">"),
            Token::new(TokenType::Comma, 1, ","),
            Token::new(TokenType::Dot, 1, "."),
            Token::new(TokenType::Plus, 1, "+"),
            Token::new(TokenType::Minus, 1, "-"),
            Token::new(TokenType::Semicolon, 1, ";"),
            Token::new(TokenType::Star, 1, "*"),
            Token::new(TokenType::Slash, 1, "/"),
            Token::new(TokenType::At, 1, "@"),
            Token::new(TokenType::Pipe, 1, "|"),
            Token::new(TokenType::Question, 1, "?"),
        ];
        assert_eq!(tokens, expected_tokens);

        let source = "=!>=<|";
        let tokens = tokenize(source).unwrap();
        let expected_tokens = vec![
            Token::new(TokenType::Equal, 1, "="),
            Token::new(TokenType::Bang, 1, "!"),
            Token::new(TokenType::GreaterEqual, 1, ">="),
            Token::new(TokenType::LeftAngle, 1, "<"),
            Token::new(TokenType::Pipe, 1, "|"),
        ];
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_string() {
        let source = "# This is a comment";
        let tokens = tokenize(source).unwrap();
        let expected_tokens = vec![Token::new(
            TokenType::Comment(" This is a comment".to_string()),
            1,
            "# This is a comment",
        )];
        assert_eq!(tokens, expected_tokens);

        let source = "'a' \"string\"";
        let tokens = tokenize(source).unwrap();
        let expected_tokens = vec![
            Token::new(TokenType::Char('a'), 1, "'a'"),
            Token::new(TokenType::String("string".to_string()), 1, "\"string\""),
        ];
        assert_eq!(tokens, expected_tokens);

        let source = "identifier keyword true false null";
        let tokens = tokenize(source).unwrap();
        let expected_tokens = vec![
            Token::new(
                TokenType::Identifier("identifier".to_string()),
                1,
                "identifier",
            ),
            Token::new(TokenType::Identifier("keyword".to_string()), 1, "keyword"),
            Token::new(TokenType::True, 1, "true"),
            Token::new(TokenType::False, 1, "false"),
            Token::new(TokenType::Null, 1, "null"),
        ];
        assert_eq!(tokens, expected_tokens);

        let source = "";
        let result = tokenize(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_number() {
        let source = "123 456.789 10'11'12'13";
        let tokens = tokenize(source).unwrap();
        let expected_tokens = vec![
            Token::new(TokenType::Integer(123), 1, "123"),
            Token::new(TokenType::Float(456.789), 1, "456.789"),
            Token::new(TokenType::Integer(10111213), 1, "10'11'12'13"),
        ];
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_failable_cases() {
        // Test for undetermined string literal
        let source = "\"This is an unterminated string";
        let result = tokenize(source);
        assert!(matches!(result, Err(LexicalError::UndeterminedStringLiteral(_))));

        // Test for undetermined char literal
        let source = "'\\x";
        let result = tokenize(source);
        assert!(matches!(result, Err(LexicalError::UndeterminedCharLiteral(_))));

        let source = "'\\\\n'";
        let result = tokenize(source);
        assert!(matches!(result, Err(LexicalError::UndeterminedCharLiteral(_))));

        // Test for empty char literal
        let source = "''";
        let result = tokenize(source);
        assert!(matches!(result, Err(LexicalError::EmptyCharDeclaration(_))));

        // Test for malformed number
        let source = "123..456";
        let result = tokenize(source);
        assert!(matches!(result, Err(LexicalError::MalformedNumber(_))));
    }
}
