#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use core::fmt;
use std::{alloc::Layout, fmt::Display, mem::transmute};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[repr(u32)]
pub enum Token {
    // Single chars
    ADD = ('+' as u32),
    MINUS = ('-' as u32),
    DIV = ('/' as u32),
    MUL = ('*' as u32),
    MOD = ('%' as u32),
    ASSIGN = ('=' as u32),
    AMP = ('&' as u32),
    PIPE = ('|' as u32),
    XOR = ('^' as u32),
    GT = ('>' as u32),
    LT = ('<' as u32),
    COLON = (',' as u32),
    SCOLON = (';' as u32),
    LPAR = ('(' as u32),
    RPAR = (')' as u32),
    LBRAC = ('[' as u32),
    RBRAC = (']' as u32),
    LCBRAC = ('{' as u32),
    RCBRAC = ('}' as u32),
    // ! single chars
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
    FIRST_UNUSED_TOKEN = 286, // ?
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::COLON => String::from(","),
                Token::SCOLON => String::from(";"),
                Token::LPAR => String::from("("),
                Token::RPAR => String::from(")"),
                Token::LBRAC => String::from("["),
                Token::RBRAC => String::from("]"),
                Token::LCBRAC => String::from("{"),
                Token::RCBRAC => String::from("}"),
                Token::ADD => String::from("+"),
                Token::MINUS => String::from("-"),
                Token::DIV => String::from("/"),
                Token::MUL => String::from("*"),
                Token::MOD => String::from("%"),
                Token::ASSIGN => String::from("="),
                Token::AMP => String::from("&"),
                Token::PIPE => String::from("|"),
                Token::XOR => String::from("^"),
                Token::GT => String::from(">"),
                Token::LT => String::from("<"),
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
        unsafe { transmute(tok_id) }
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
