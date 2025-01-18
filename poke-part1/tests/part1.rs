use common::disassemble_test;
use common::simulate_test;
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
    disassemble_test("listing_0039_more_movs", *TEST_DIR, false);
}

#[test]
fn listing_0040_challenge_movs() {
    disassemble_test("listing_0040_challenge_movs", *TEST_DIR, false);
}

#[test]
fn listing_0041_add_sub_cmp_jnz() {
    disassemble_test("listing_0041_add_sub_cmp_jnz", *TEST_DIR, false);
}

#[test]
fn listing_0043_immediate_movs() {
    simulate_test("listing_0043_immediate_movs", *TEST_DIR, true, false, None);
}

#[test]
fn listing_0044_register_movs() {
    simulate_test("listing_0044_register_movs", *TEST_DIR, true, false, None);
}

#[test]
fn listing_0045_challenge_register_movs() {
    simulate_test(
        "listing_0045_challenge_register_movs",
        *TEST_DIR,
        false,
        false,
        None,
    );
}

#[test]
fn listing_0046_add_sub_cmp() {
    simulate_test("listing_0046_add_sub_cmp", *TEST_DIR, false, false, None);
}

#[test]
fn listing_0047_challenge_flags() {
    simulate_test("listing_0047_challenge_flags", *TEST_DIR, true, false, None);
}

#[test]
fn listing_0048_ip_register() {
    simulate_test("listing_0048_ip_register", *TEST_DIR, true, true, None);
}

#[test]
fn listing_0049_conditional_jumps() {
    simulate_test(
        "listing_0049_conditional_jumps",
        *TEST_DIR,
        false,
        true,
        None,
    );
}

#[test]
fn listing_0050_challenge_jumps() {
    simulate_test("listing_0050_challenge_jumps", *TEST_DIR, false, true, None);
}

#[test]
fn listing_0051_memory_mov() {
    simulate_test("listing_0051_memory_mov", *TEST_DIR, false, true, None);
}

#[test]
fn listing_0052_memory_add_loop() {
    simulate_test("listing_0052_memory_add_loop", *TEST_DIR, false, true, None);
}

#[test]
fn listing_0053_add_loop_challenge() {
    simulate_test(
        "listing_0053_add_loop_challenge",
        *TEST_DIR,
        false,
        true,
        None,
    );
}

#[test]
fn listing_0054_draw_rectangle() {
    simulate_test("listing_0054_draw_rectangle", *TEST_DIR, false, true, None);
}

#[test]
fn listing_0055_challenge_rectangle() {
    simulate_test(
        "listing_0055_challenge_rectangle",
        *TEST_DIR,
        false,
        true,
        None,
    );
}

#[test]
fn listing_0056_estimating_cycles() {
    simulate_test(
        "listing_0056_estimating_cycles",
        *TEST_DIR,
        false,
        true,
        Some(2),
    );
}

#[test]
fn listing_0057_challenge_cycles() {
    simulate_test(
        "listing_0057_challenge_cycles",
        *TEST_DIR,
        false,
        true,
        Some(3),
    );
}
