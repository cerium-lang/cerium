use cerium_position::Position;

use std::str::Chars;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    pub position: Position,
}

impl<'a> Cursor<'a> {
    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }
}

impl<'a> From<Chars<'a>> for Cursor<'a> {
    fn from(chars: Chars<'a>) -> Self {
        Cursor {
            chars,
            position: Position::default(),
        }
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.chars.next();

        if ch.is_some_and(|c| c == '\n') {
            self.position.line += 1;
        } else if ch.is_some() {
            self.position.column += 1;
        }

        ch
    }
}
