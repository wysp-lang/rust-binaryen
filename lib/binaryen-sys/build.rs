extern crate bindgen;
extern crate cmake;

use std::env;
use std::path::PathBuf;

use bindgen::CargoCallbacks;

fn main() {
    let dst = cmake::Config::new("binaryen")
        .generator("Unix Makefiles")
        .build();

    let header_file_path = dst.join("include/binaryen-c.h");
    let header_file = header_file_path.to_str().unwrap();

    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header(header_file)
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
