#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::alloc::Layout;

use crate::{LexerIteratorItem, TokenKind};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub struct StbLexer<'source> {
    ptr: *mut stb_lexer,
    // Layout of what's pointed to *by the ptr*, not this struct itself
    layout: Layout,
    _store_length: i32,
    _string_store: String,
    input: &'source str,
}

impl<'source> StbLexer<'source> {
    pub fn new(code: &'source str) -> Self {
        let _store_length: i32 = 0x10000;
        let mut _string_store: String = String::with_capacity(_store_length as usize);
        let layout = Layout::new::<stb_lexer>();

        unsafe {
            let ptr = std::alloc::alloc(layout) as *mut stb_lexer;

            stb_c_lexer_init(
                ptr,
                code.as_ptr() as *const i8,
                code.as_ptr().add(code.len()) as *const i8,
                _string_store.as_mut_ptr() as *mut i8,
                _store_length,
            );

            StbLexer {
                ptr,
                layout,
                _store_length,
                _string_store,
                input: code,
            }
        }
    }
}

impl Iterator for StbLexer<'_> {
    type Item = super::LexerIteratorItem;

    fn next(&mut self) -> Option<Self::Item> {
        let token_id = unsafe { stb_c_lexer_get_token(self.ptr) };

        match token_id as u32 {
            CLEX_eof => None,
            CLEX_parse_error => Some(Err(unimplemented!())),
            x if x < 256 => match x as u8 {
                b'+' => Some(Ok((0, TokenKind::Plus, 0))),
                b'-' => Some(Ok((0, TokenKind::Minus, 0))),
                b'/' => Some(Ok((0, TokenKind::Div, 0))),
                b'*' => Some(Ok((0, TokenKind::Star, 0))),
                b'%' => Some(Ok((0, TokenKind::Mod, 0))),
                b'=' => Some(Ok((0, TokenKind::Assign, 0))),
                b'&' => Some(Ok((0, TokenKind::Ampersand, 0))),
                b'|' => Some(Ok((0, TokenKind::BinaryOr, 0))),
                b'^' => Some(Ok((0, TokenKind::Xor, 0))),
                b'>' => Some(Ok((0, TokenKind::GreaterThan, 0))),
                b'<' => Some(Ok((0, TokenKind::LessThan, 0))),
                b',' => Some(Ok((0, TokenKind::Comma, 0))),
                b';' => Some(Ok((0, TokenKind::Semicolon, 0))),
                b'(' => Some(Ok((0, TokenKind::LeftParen, 0))),
                b')' => Some(Ok((0, TokenKind::RightParen, 0))),
                b'[' => Some(Ok((0, TokenKind::LeftBracket, 0))),
                b']' => Some(Ok((0, TokenKind::RightBracket, 0))),
                b'{' => Some(Ok((0, TokenKind::LeftBrace, 0))),
                b'}' => Some(Ok((0, TokenKind::RightBrace, 0))),
                _ => Some(Err(unimplemented!())),
            },
            CLEX_intlit => Some(Ok((
                0,
                TokenKind::Constant(crate::ConstantKind::Integer(todo!())),
                0,
            ))),
            CLEX_floatlit => Some(Ok((
                0,
                TokenKind::Constant(crate::ConstantKind::Float(todo!())),
                0,
            ))),
            CLEX_id => Some(Ok((0, TokenKind::Identifier(todo!()), 0))),
            CLEX_dqstring => Some(Ok((0, todo!("What is that exactly ?"), 0))),

            /* TODO these are not treated (I mean, even less so than the ones before)
            CLEX_charlit => Some(Ok((0, todo!(), 0))),
            CLEX_EQ => Some(Ok((0, todo!(), 0))),
            CLEX_NOTEQ => Some(Ok((0, todo!(), 0))),
            CLEX_LESSEQ => Some(Ok((0, todo!(), 0))),
            CLEX_GREATEREQ => Some(Ok((0, todo!(), 0))),
            CLEX_ANDAND => Some(Ok((0, todo!(), 0))),
            CLEX_OROR => Some(Ok((0, todo!(), 0))),
            CLEX_SHL => Some(Ok((0, todo!(), 0))),
            CLEX_SHR => Some(Ok((0, todo!(), 0))),
            CLEX_PLUSPLUS => Some(Ok((0, todo!(), 0))),
            CLEX_MINUSMINUS => Some(Ok((0, todo!(), 0))),
            CLEX_PLUSEQ => Some(Ok((0, todo!(), 0))),
            CLEX_MINUSEQ => Some(Ok((0, todo!(), 0))),
            CLEX_MULEQ => Some(Ok((0, todo!(), 0))),
            CLEX_DIVEQ => Some(Ok((0, todo!(), 0))),
            CLEX_MODEQ => Some(Ok((0, todo!(), 0))),
            CLEX_ANDEQ => Some(Ok((0, todo!(), 0))),
            CLEX_OREQ => Some(Ok((0, todo!(), 0))),
            CLEX_XOREQ => Some(Ok((0, todo!(), 0))),
            CLEX_ARROW => Some(Ok((0, todo!(), 0))),
            CLEX_EQARROW => Some(Ok((0, todo!(), 0))),
            CLEX_SHLEQ => Some(Ok((0, todo!(), 0))),
            CLEX_SHREQ => Some(Ok((0, todo!(), 0))),
            CLEX_FIRST_UNUSED_TOKEN => Some(Ok((0, todo!(), 0))),
            */
            _ => Some(Err(unimplemented!(
                "Getting location might not be trivial yet"
            ))),
        }
    }
}

impl<'source> Drop for StbLexer<'source> {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ptr as *mut u8, self.layout);
        };
    }
}
