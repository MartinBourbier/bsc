use crate::{ConstantKind, ErrorKind, Lexer, LexerIteratorItem, LexingError, Token, TokenKind};

/// A dummy lexer that tokenizes **only** integer constants.
///
/// This lexer's purpose is to demonstrate the use of the lexer interface.
/// It produces tokens and somewhat useful diagnostics on error.
pub struct DummyLexer<'source> {
    source: &'source str,
    cursor: usize,
}

impl Iterator for DummyLexer<'_> {
    type Item = LexerIteratorItem;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.source.len() {
            return None; // End of input
        }

        let start = self.cursor;
        let next_space = self.source[self.cursor..]
            .find(' ')
            .unwrap_or(self.source.len() - self.cursor);

        let token_str = &self.source[self.cursor..self.cursor + next_space];
        self.cursor += next_space + 1;
        let token_str = token_str.trim();

        if token_str.is_empty() {
            return None;
        }

        if let Ok(number) = token_str.parse::<u64>() {
            Some(Ok(Token {
                kind: TokenKind::Constant(ConstantKind::Integer(number)),
                span: start..(self.cursor - 1),
            }))
        } else {
            Some(Err(LexingError {
                kind: ErrorKind::InvalidIntegerConstant,
                position: start..(self.cursor - 1),
                message: format!("Invalid token: {}", token_str),
            }))
        }
    }
}

impl<'source> Lexer<'source> for DummyLexer<'source> {
    fn new(input: &'source str) -> Self {
        DummyLexer {
            source: input,
            cursor: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Lexer;

    macro_rules! spanned_token {
        ($kind:expr,$span:expr) => {
            Token {
                kind: $kind,
                span: $span,
            }
        };
    }

    macro_rules! constant_int {
        ($value:expr) => {
            TokenKind::Constant(ConstantKind::Integer($value))
        };
    }

    #[test]
    fn test_dummy_lexer() {
        let input = "123 456 789";
        let mut lexer = DummyLexer::new(input);

        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(123), 0..3)))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(456), 4..7)))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(789), 8..11)))
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_invalid_token() {
        let input = "123 abc";
        let mut lexer = DummyLexer::new(input);

        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(123), 0..3)))
        );
        assert_eq!(
            lexer.next(),
            Some(Err(LexingError {
                kind: ErrorKind::InvalidIntegerConstant,
                position: 4..7,
                message: "Invalid token: abc".to_string(),
            }))
        );
    }

    #[test]
    fn test_trailing_spaces() {
        let input = "123 456 789   ";
        let mut lexer = DummyLexer::new(input);

        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(123), 0..3)))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(456), 4..7)))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(spanned_token!(constant_int!(789), 8..11)))
        );
        assert_eq!(lexer.next(), None);
    }
}
