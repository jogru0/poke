use std::{fs::create_dir_all, sync::LazyLock};

use common::disassemble_test;

mod common;

static TEST_DIR: LazyLock<&str> = LazyLock::new(|| {
    let result = "disassembly";
    create_dir_all(format!("out/{}", result))
        .expect("unable to create output directory for the tests");
    result
});

#[test]
fn mov_all_kinds_of_displacements() {
    disassemble_test("mov_all_kinds_of_displacements", *TEST_DIR, true);
}

#[test]
fn mov_immediate_accumulator() {
    disassemble_test("mov_immediate_accumulator", *TEST_DIR, true);
}

#[test]
fn add_sign_extension() {
    disassemble_test("add_sign_extension", *TEST_DIR, true);
}
