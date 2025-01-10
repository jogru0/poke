use common::disassemble_test;
use std::{fs::create_dir_all, sync::LazyLock};

mod common;

static TEST_DIR: LazyLock<&str> = LazyLock::new(|| {
    let result = "computer_enhance/part1";
    create_dir_all(format!("out/{}", result))
        .expect("unable to create output directory for the tests");
    result
});

#[test]
fn listing_0037_single_register_mov() {
    disassemble_test("listing_0037_single_register_mov", *TEST_DIR, true);
}

#[test]
fn listing_0038_many_register_mov() {
    disassemble_test("listing_0038_many_register_mov", *TEST_DIR, true);
}

#[test]
fn listing_0039_more_movs() {
    disassemble_test("listing_0039_more_movs", *TEST_DIR, true);
}

#[test]
fn listing_0040_challenge_movs() {
    disassemble_test("listing_0040_challenge_movs", *TEST_DIR, true);
}

#[test]
fn listing_0041_add_sub_cmp_jnz() {
    disassemble_test("listing_0041_add_sub_cmp_jnz", *TEST_DIR, false);
}
