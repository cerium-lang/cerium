mod cursor;
pub mod tokens;

use cursor::Cursor;
use tokens::*;

use cerium_errors::Error;

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Lexer {
        Lexer {
            cursor: Cursor::from(source_code.chars()),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        macro_rules! mini_condition {
            ($next: expr, $if: expr, $else: expr) => {
                if self.cursor.peek().is_some_and(|c| c == $next) {
                    self.cursor.next();
                    $if
                } else {
                    $else
                }
            };
        }

        self.skip_whitespace();

        let Some(ch) = self.cursor.next() else {
            return Ok(Token::EOF);
        };

        Ok(match ch {
            '=' => mini_condition!(
                '=',
                Token::Operator(Operator::Eq),
                Token::Delimiter(Delimiter::Assign)
            ),
            '!' => mini_condition!(
                '=',
                Token::Operator(Operator::NotEq),
                Token::Operator(Operator::Bang)
            ),
            '+' => Token::Operator(Operator::Plus),
            '-' => Token::Operator(Operator::Minus),
            '/' => Token::Operator(Operator::ForwardSlash),
            '*' => Token::Operator(Operator::Star),
            '>' => Token::Operator(Operator::GreaterThan),
            '<' => Token::Operator(Operator::LessThan),
            ':' => Token::Delimiter(Delimiter::Colon),
            ';' => Token::Delimiter(Delimiter::Semicolon),
            ',' => Token::Delimiter(Delimiter::Comma),
            '(' => Token::Delimiter(Delimiter::LParen),
            ')' => Token::Delimiter(Delimiter::RParen),
            '[' => Token::Delimiter(Delimiter::LBracket),
            ']' => Token::Delimiter(Delimiter::RBracket),
            '{' => Token::Delimiter(Delimiter::LBrace),
            '}' => Token::Delimiter(Delimiter::RBrace),

            _ => {
                return Err(Error::invalid(
                    &self.cursor.position,
                    format!("token '{ch}'").as_str(),
                ))
            }
        })
    }

    fn skip_whitespace(&mut self) {
        while self.cursor.peek().is_some_and(|c| c.is_whitespace()) {
            self.cursor.next();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operators_and_delimiters_tokenization() {
        let input = "+ - * / = ! < > : , ; ( ) [ ] { } == !=";

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Operator(Operator::Plus),
            Token::Operator(Operator::Minus),
            Token::Operator(Operator::Star),
            Token::Operator(Operator::ForwardSlash),
            Token::Delimiter(Delimiter::Assign),
            Token::Operator(Operator::Bang),
            Token::Operator(Operator::LessThan),
            Token::Operator(Operator::GreaterThan),
            Token::Delimiter(Delimiter::Colon),
            Token::Delimiter(Delimiter::Comma),
            Token::Delimiter(Delimiter::Semicolon),
            Token::Delimiter(Delimiter::LParen),
            Token::Delimiter(Delimiter::RParen),
            Token::Delimiter(Delimiter::LBracket),
            Token::Delimiter(Delimiter::RBracket),
            Token::Delimiter(Delimiter::LBrace),
            Token::Delimiter(Delimiter::RBrace),
            Token::Operator(Operator::Eq),
            Token::Operator(Operator::NotEq),
            Token::EOF,
        ];

        for expected_token in expected_tokens {
            assert_eq!(lexer.next_token(), Ok(expected_token));
        }
    }
}
