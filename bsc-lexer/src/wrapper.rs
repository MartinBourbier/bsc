#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use core::fmt;
use std::{alloc::Layout, fmt::Display, mem::transmute};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[repr(u32)]
pub enum Token {
    SINGLE_CHAR, // 0..255
    EOF = 256,
    PARSE_ERROR = 257,
    INTLIT = 258,
    FLOATLIT = 259,
    ID = 260,
    DQSTRING = 261,
    SQSTRING = 262,
    CHARLIT = 263,
    EQ = 264,
    NOTEQ = 265,
    LESSEQ = 266,
    GREATEREQ = 267,
    ANDAND = 268,
    OROR = 269,
    SHL = 270,
    SHR = 271,
    PLUSPLUS = 272,
    MINUSMINUS = 273,
    PLUSEQ = 274,
    MINUSEQ = 275,
    MULEQ = 276,
    DIVEQ = 277,
    MODEQ = 278,
    ANDEQ = 279,
    OREQ = 280,
    XOREQ = 281,
    ARROW = 282,
    EQARROW = 283,
    SHLEQ = 284,
    SHREQ = 285,
    FIRST_UNUSED_TOKEN = 286,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::SINGLE_CHAR => String::from("SINGLE_CHAR"),
                Token::EOF => String::from("EOF"),
                Token::PARSE_ERROR => String::from("PARSE_ERROR"),
                Token::INTLIT => String::from("INTLIT"),
                Token::FLOATLIT => String::from("FLOATLIT"),
                Token::ID => String::from("ID"),
                Token::DQSTRING => String::from("DQSTRING"),
                Token::SQSTRING => String::from("SQSTRING"),
                Token::CHARLIT => String::from("CHARLIT"),
                Token::EQ => String::from("EQ"),
                Token::NOTEQ => String::from("NOTEQ"),
                Token::LESSEQ => String::from("LESSEQ"),
                Token::GREATEREQ => String::from("GREATEREQ"),
                Token::ANDAND => String::from("ANDAND"),
                Token::OROR => String::from("OROR"),
                Token::SHL => String::from("SHL"),
                Token::SHR => String::from("SHR"),
                Token::PLUSPLUS => String::from("PLUSPLUS"),
                Token::MINUSMINUS => String::from("MINUSMINUS"),
                Token::PLUSEQ => String::from("PLUSEQ"),
                Token::MINUSEQ => String::from("MINUSEQ"),
                Token::MULEQ => String::from("MULEQ"),
                Token::DIVEQ => String::from("DIVEQ"),
                Token::MODEQ => String::from("MODEQ"),
                Token::ANDEQ => String::from("ANDEQ"),
                Token::OREQ => String::from("OREQ"),
                Token::XOREQ => String::from("XOREQ"),
                Token::ARROW => String::from("ARROW"),
                Token::EQARROW => String::from("EQARROW"),
                Token::SHLEQ => String::from("SHLEQ"),
                Token::SHREQ => String::from("SHREQ"),
                Token::FIRST_UNUSED_TOKEN => String::from("FIRST_UNUSED_TOKEN"),
            }
        )
    }
}

pub struct Lexer {
    ptr: *mut stb_lexer,
    // Layout of what's pointed to *by the ptr*, not this struct itself
    layout: Layout,
    _store_length: i32,
    _string_store: String,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
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

            Lexer {
                ptr,
                layout,
                _store_length,
                _string_store,
            }
        }
    }

    pub fn next_token(&self) -> i32 {
        unsafe { stb_c_lexer_get_token(self.ptr) }
    }

    fn token_from_bindgen(tok_id: _bindgen_ty_1) -> Token {
        match tok_id {
            0..255 => Token::SINGLE_CHAR,
            _ => unsafe { transmute(tok_id) },
        }
    }

    pub fn get_token(&self) -> Token {
        Lexer::token_from_bindgen(unsafe { (*self.ptr).token as u32 })
    }
}

impl Drop for Lexer {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ptr as *mut u8, self.layout);
        };
    }
}
