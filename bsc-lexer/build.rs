use std::env;
use std::path::PathBuf;

fn main() {
    let libdir_path = PathBuf::from("stb_c_lexer/")
        .canonicalize()
        .expect("cannot canonicalize path");

    let headers_path = libdir_path.join("stb_c_lexer.h");
    let headers_path_str = headers_path.to_str().expect("Path is not a valid string");

    let outdir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let obj_path = outdir.join("stb_c_lexer.o");
    let lib_path = outdir.join("libstb_c_lexer.a");

    println!("cargo:rustc-link-search={}", libdir_path.to_str().unwrap());
    println!("cargo:rustc-link-search={}", outdir.to_str().unwrap());

    println!("cargo:rustc-link-lib=stb_c_lexer");

    if !std::process::Command::new("clang")
        .arg("-c")
        .arg("-o")
        .arg(&obj_path)
        .arg(libdir_path.join("stb_c_lexer.c"))
        .output()
        .expect("could not spawn `clang`")
        .status
        .success()
    {
        panic!("could not compile object file");
    }

    if !std::process::Command::new("ar")
        .arg("rcs")
        .arg(lib_path)
        .arg(obj_path)
        .output()
        .expect("could not spawn `ar`")
        .status
        .success()
    {
        panic!("could not emit library file");
    }

    let bindings = bindgen::Builder::default()
        .header(headers_path_str)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs");
    bindings
        .write_to_file(out_path)
        .expect("Couldn't write bindings!");
}
