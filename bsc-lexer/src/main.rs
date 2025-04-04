mod wrapper;

use std::error::Error;
use wrapper::Lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let code = "int main(void) { int a = 8; return a; }";

    {
        let lexer = Lexer::new(code);

        // Get token not yet wrapped
        while lexer.next_token() != 0 {
            println!("{}", lexer.get_token());
        }
    } // Lexer implements Drop trait, so everything should be freed here

    Ok(())
}
