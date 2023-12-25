use cerium_ast::*;

use cerium_errors::Error;

use cerium_lexer::tokens::*;
use cerium_lexer::Lexer;

use cerium_position::Position;

use cerium_ty::Ty;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Result<Package, Error> {
        let mut declarations = Vec::new();

        while self.peek_token()? != Token::EOF {
            declarations.push(self.parse_declaration()?);
        }

        Ok(Package {
            name: String::new(),
            declarations,
            position: self.lexer.cursor.position,
        })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, Error> {
        match self.next_token()? {
            Token::Identifier(symbol) => match symbol.as_str() {
                "fn" => self.parse_function_declaration(),
                _ => Err(Error::expected(
                    self.lexer.cursor.position,
                    "top-level declaration",
                )),
            },
            _ => Err(Error::expected(
                self.lexer.cursor.position,
                "top-level declaration",
            )),
        }
    }

    fn parse_function_declaration(&mut self) -> Result<Declaration, Error> {
        let position = self.lexer.cursor.position;

        let name = match self.next_token()? {
            Token::Identifier(symbol) => self.parse_identifier(symbol, position)?,
            _ => return Err(Error::expected(position, "an identifier")),
        };

        let parameters = self.parse_function_parameters()?;

        let return_type = self.parse_ty()?;

        if !self.expect(Token::Delimiter(Delimiter::LBrace))? {
            return Err(Error::expected(
                self.lexer.cursor.position,
                "function body starts with '{'",
            ));
        }

        let body = self.parse_nodes_while(|token| {
            token != Token::Delimiter(Delimiter::RBrace) && token != Token::EOF
        })?;

        self.expect(Token::Delimiter(Delimiter::RBrace))?;

        let signature = FunctionSignature {
            parameters,
            return_type,
        };

        Ok(Declaration::FunctionDeclaration {
            name,
            signature,
            body,
            position,
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<FunctionParameter>, Error> {
        if !self.expect(Token::Delimiter(Delimiter::LParen))? {
            return Err(Error::expected(
                self.lexer.cursor.position,
                "function parameters starts with '('",
            ));
        }

        let mut parameters = Vec::new();

        while !self.expect(Token::Delimiter(Delimiter::RParen))? {
            parameters.push(self.parse_function_parameter()?);

            if !self.expect(Token::Delimiter(Delimiter::Comma))?
                && self.peek_token()? != Token::Delimiter(Delimiter::RParen)
            {
                return Err(Error::expected(self.lexer.cursor.position, "a comma"));
            }
        }

        Ok(parameters)
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, Error> {
        let position = self.lexer.cursor.position;

        let name = match self.next_token()? {
            Token::Identifier(symbol) => self.parse_identifier(symbol, position)?,
            _ => return Err(Error::expected(position, "an identifier")),
        };

        let expected_type = self.parse_ty()?;

        Ok(FunctionParameter {
            name,
            expected_type,
            position,
        })
    }

    fn parse_nodes_while(&mut self, condition: impl Fn(Token) -> bool) -> Result<Vec<Node>, Error> {
        let mut nodes = Vec::new();

        while condition(self.peek_token()?) {
            nodes.push(self.parse_node()?);
        }

        Ok(nodes)
    }

    fn parse_node(&mut self) -> Result<Node, Error> {
        match self.peek_token()? {
            _ => self.parse_expr(),
        }
    }

    fn parse_expr(&mut self) -> Result<Node, Error> {
        Ok(Node::Expr(self.parse_expr_kind()?.into()))
    }

    fn parse_expr_kind(&mut self) -> Result<ExprKind, Error> {
        let position = self.lexer.cursor.position;

        match self.next_token()? {
            Token::Identifier(symbol) => self.parse_identifier(symbol, position),
            _ => Err(Error::invalid(position, "expression")),
        }
    }

    fn parse_identifier(&mut self, symbol: String, position: Position) -> Result<ExprKind, Error> {
        Ok(ExprKind::Identifier { symbol, position })
    }

    fn parse_ty(&mut self) -> Result<Ty, Error> {
        let position = self.lexer.cursor.position;

        Ok(match self.next_token()? {
            Token::Identifier(symbol) => match symbol.as_str() {
                "void" => Ty::Void,
                "string" => Ty::String,
                "char" => Ty::Char,
                "int" => Ty::Int,
                "float" => Ty::Float,
                _ => return Err(Error::invalid(position, "type")),
            },
            _ => return Err(Error::invalid(position, "type")),
        })
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        self.lexer.next_token()
    }

    fn peek_token(&self) -> Result<Token, Error> {
        self.lexer.clone().next_token()
    }

    fn expect(&mut self, token: Token) -> Result<bool, Error> {
        if self.peek_token()? == token {
            self.next_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_declaration() {
        let input = r#"
        fn main() void {

        }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        assert_eq!(
            parser.parse(),
            Ok(Package {
                name: "".to_string(),
                declarations: vec![Declaration::FunctionDeclaration {
                    name: ExprKind::Identifier {
                        symbol: "main".to_string(),
                        position: Position {
                            line: 2,
                            column: 11
                        }
                    },
                    signature: FunctionSignature {
                        parameters: Vec::new(),
                        return_type: Ty::Void
                    },
                    body: Vec::new(),
                    position: Position {
                        line: 2,
                        column: 11
                    }
                }],
                position: Position {
                    line: 4,
                    column: 10
                }
            })
        );
    }
}
