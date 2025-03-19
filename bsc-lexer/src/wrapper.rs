#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::alloc::Layout;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub struct Lexer {
    pub ptr: *mut stb_lexer,
    // Layout of what's pointed to *by the ptr*, not this struct itself
    pub layout: Layout,
    store_length: i32,
    string_store: String,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let store_length: i32 = 0x10000;
        let mut string_store: String = String::with_capacity(store_length as usize);
        let layout = Layout::new::<stb_lexer>();

        unsafe {
            let ptr = std::alloc::alloc(layout) as *mut stb_lexer;

            stb_c_lexer_init(
                ptr,
                code.as_ptr() as *const i8,
                code.as_ptr().add(code.len()) as *const i8,
                string_store.as_mut_ptr() as *mut i8,
                store_length,
            );

            Lexer {
                ptr,
                layout,
                store_length,
                string_store,
            }
        }
    }
}

impl Drop for Lexer {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ptr as *mut u8, self.layout);
        };
    }
}
