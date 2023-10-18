extern crate bindgen;
extern crate cmake;

use std::env;
use std::path::PathBuf;

use bindgen::CargoCallbacks;

fn main() {
    let dst = cmake::Config::new("binaryen")
        .generator("Unix Makefiles")
        .build();

    let src = PathBuf::from("binaryen/src");

    let c_header_path = src.join("binaryen-c.h");
    let wasm_header_path = src.join("wasm.h");

    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .clang_args(&["-I", src.to_str().unwrap(), "-xc++", "--std=c++17"])
        .header(c_header_path.to_str().unwrap())
        .header(wasm_header_path.to_str().unwrap())
        .opaque_type("std::.*")
        .allowlist_file(".*binaryen-c.h")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(CargoCallbacks))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs");
    bindings.write_to_file(out_path).unwrap();

    let libdir = dst.join("lib64");

    println!("cargo:rustc-link-search=native={}", libdir.display());
    println!("cargo:rustc-link-lib={}", "binaryen");
}
