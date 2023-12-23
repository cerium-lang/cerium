#[derive(Debug, PartialEq)]
pub enum Token {
    // Literals
    Identifier(String),
    String(String),
    Char(char),
    Int(i64),
    Float(f64),

    // Operators
    Operator(Operator),

    // Delimitiers
    Delimiter(Delimiter),

    // Special
    EOF,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Eq,
    NotEq,
    Bang,
    Plus,
    Minus,
    ForwardSlash,
    Star,
    GreaterThan,
    LessThan,
}

#[derive(Debug, PartialEq)]
pub enum Delimiter {
    Assign,
    Colon,
    Semicolon,
    Comma,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
}

impl Token {
    pub fn as_char(&self) -> char {
        match self {
            Token::Operator(operator) => match operator {
                Operator::Bang => '!',
                Operator::Plus => '+',
                Operator::Minus => '-',
                Operator::ForwardSlash => '/',
                Operator::Star => '*',
                Operator::GreaterThan => '<',
                Operator::LessThan => '>',

                _ => '\0',
            },

            Token::Delimiter(delimiter) => match delimiter {
                Delimiter::Assign => '=',
                Delimiter::Colon => ':',
                Delimiter::Semicolon => ';',
                Delimiter::Comma => ',',
                Delimiter::LParen => '(',
                Delimiter::RParen => ')',
                Delimiter::LBracket => '[',
                Delimiter::RBracket => ']',
                Delimiter::LBrace => '{',
                Delimiter::RBrace => '}',
            },

            Token::Char(value) => *value,

            _ => '\0',
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::Identifier(symbol) => symbol.clone(),
            Token::String(value) => value.clone(),
            Token::Char(value) => value.to_string(),
            Token::Int(value) => value.to_string(),
            Token::Float(value) => value.to_string(),

            Token::Operator(operator) => match operator {
                Operator::Eq => String::from("=="),
                Operator::NotEq => String::from("!="),
                Operator::Bang => String::from("!"),
                Operator::Plus => String::from("+"),
                Operator::Minus => String::from("-"),
                Operator::ForwardSlash => String::from("/"),
                Operator::Star => String::from("*"),
                Operator::GreaterThan => String::from("<"),
                Operator::LessThan => String::from(">"),
            },

            Token::Delimiter(delimiter) => match delimiter {
                Delimiter::Assign => String::from("="),
                Delimiter::Colon => String::from(":"),
                Delimiter::Semicolon => String::from(";"),
                Delimiter::Comma => String::from(","),
                Delimiter::LParen => String::from("("),
                Delimiter::RParen => String::from(")"),
                Delimiter::LBracket => String::from("["),
                Delimiter::RBracket => String::from("]"),
                Delimiter::LBrace => String::from("{"),
                Delimiter::RBrace => String::from("}"),
            },

            Token::EOF => String::from("EOF"),
        }
    }
}
