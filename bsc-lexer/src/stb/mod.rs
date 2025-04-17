#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::alloc::Layout;

use crate::LexerIteratorItem;

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
    type Item = LexerIteratorItem;

    fn next(&mut self) -> Option<Self::Item> {
        let token_id = unsafe { stb_c_lexer_get_token(self.ptr) };

        match token_id as u32 {
            x if x < 256 => match x as u8 {
                b'+' => todo!(),
                b'-' => todo!(),
                _ => todo!(),
            },
            CLEX_intlit => todo!(),
            CLEX_floatlit => todo!(),
            CLEX_id => todo!(),
            // and many others
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
