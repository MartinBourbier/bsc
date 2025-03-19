mod wrapper;

use std::error::Error;
use wrapper::{Lexer, stb_c_lexer_get_token};

fn main() -> Result<(), Box<dyn Error>> {
    let code = "int a = 0;";

    {
        let lexer = Lexer::new(code);

        // Get token not yet wrapped
        unsafe {
            while stb_c_lexer_get_token(lexer.ptr) != 0 {
                println!("{}", (*lexer.ptr).token);
            }
        }
    } // Lexer implements Drop trait, so everything should be freed here

    Ok(())
}
