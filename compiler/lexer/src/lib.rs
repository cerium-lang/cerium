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
            '/' => mini_condition!(
                '/',
                self.skip_comment()?,
                Token::Operator(Operator::ForwardSlash)
            ),
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

            '"' => self.tokenize_string(),
            '\'' => self.tokenize_char()?,
            'a'..='z' | 'A'..='Z' | '_' => self.tokenize_identifier(ch),
            '0'..='9' => self.tokenize_number(ch)?,

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

    fn skip_comment(&mut self) -> Result<Token, Error> {
        while self.cursor.peek().is_some_and(|c| c != '\n') {
            self.cursor.next();
        }

        self.next_token()
    }

    fn tokenize_string(&mut self) -> Token {
        let mut literal = String::new();

        while let Some(ch) = self.cursor.next() {
            match ch {
                '"' => break,
                _ => literal.push(ch),
            }
        }

        Token::String(literal)
    }

    fn tokenize_char(&mut self) -> Result<Token, Error> {
        let mut literal = String::new();

        while let Some(ch) = self.cursor.next() {
            match ch {
                '\'' => break,
                _ => literal.push(ch),
            }
        }

        if literal.len() > 1 || literal.is_empty() {
            return Err(Error::expected(&self.cursor.position, "a single character"));
        }

        Ok(Token::Char(literal.pop().unwrap()))
    }

    fn tokenize_identifier(&mut self, first_char: char) -> Token {
        let mut literal = String::from(first_char);

        while let Some(ch) = self.cursor.peek() {
            match ch {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => literal.push(ch),
                _ => break,
            }

            self.cursor.next();
        }

        Token::Identifier(literal)
    }

    fn tokenize_number(&mut self, first_char: char) -> Result<Token, Error> {
        let mut literal = String::from(first_char);

        while let Some(ch) = self.cursor.peek() {
            match ch {
                '0'..='9' | '.' => literal.push(ch),
                _ => break,
            }

            self.cursor.next();
        }

        if let Ok(integer) = literal.parse::<i64>() {
            Ok(Token::Int(integer))
        } else if let Ok(float) = literal.parse::<f64>() {
            Ok(Token::Float(float))
        } else {
            Err(Error::invalid(&self.cursor.position, "number"))
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

    #[test]
    fn test_literals_tokenization() {
        let input = r#"hello "Hello, World!" 5 5.5 'H'"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("hello".to_string()),
            Token::String("Hello, World!".to_string()),
            Token::Int(5),
            Token::Float(5.5),
            Token::Char('H'),
            Token::EOF,
        ];

        for expected_token in expected_tokens {
            assert_eq!(lexer.next_token(), Ok(expected_token));
        }
    }
    
    #[test]
    fn test_comments() {
        let input = r#"
        // This is a comment
        // This is also a comment
        "Now it's not"
        "#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::String("Now it's not".to_string()),
            Token::EOF,
        ];

        for expected_token in expected_tokens {
            assert_eq!(lexer.next_token(), Ok(expected_token));
        }
    }
}
