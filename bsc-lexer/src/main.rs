#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::{alloc::Layout, error::Error, fmt::Debug};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

fn main() -> Result<(), Box<dyn Error>> {
    let code = "int a = 0;";
    let store_length: i32 = 0x10000;
    let mut string_store: String = String::with_capacity(store_length as usize);
    println!("{}", "Begin");

    unsafe {
        let layout = Layout::new::<stb_lexer>();
        let ptr = std::alloc::alloc(layout) as *mut stb_lexer;

        stb_c_lexer_init(
            ptr,
            code.as_ptr() as *const i8,
            code.as_ptr().add(code.len()) as *const i8,
            string_store.as_mut_ptr() as *mut i8,
            store_length,
        );

        while stb_c_lexer_get_token(ptr) != 0 {
            println!("{}", (*ptr).token);
        }

        std::alloc::dealloc(ptr as *mut u8, layout);
    }

    println!("{}", "End");
    Ok(())
}
