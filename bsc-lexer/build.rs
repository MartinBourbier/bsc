use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    println!("cargo:rustc-link-lib=bz2");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    let _ = Command::new("gcc")
        .arg("-shared")
        .arg("-o")
        .arg(out_path.join("libstb_c_lexer.so"))
        .arg("src/stb_c_lexer.c")
        .output()
        .expect("could not spawn `ar`")
        .status
        .success();
    println!(
        "{}",
        format!(
            "{}{}",
            "cargo:rustc-link-search=",
            out_path.to_str().unwrap()
        )
    );

    let bindings = bindgen::Builder::default()
        .header("src/stb_c_lexer.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("Unable to generate bindings");

    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
