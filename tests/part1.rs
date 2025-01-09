use pretty_assertions::assert_eq;
use std::{
    fs::{create_dir_all, read_to_string},
    sync::LazyLock,
};

use poke::intel_8086::disassemble;

fn read_to_string_and_remove_fluff(path: &str) -> String {
    let content = read_to_string(path).unwrap();

    let mut result = String::new();
    for line in content.lines() {
        if line.is_empty() || line.starts_with(';') {
            continue;
        }

        result += line;
        result.push('\n');
    }
    result
}

static OUTPUT_DIR: LazyLock<&str> = LazyLock::new(|| {
    let result = "out/part1";
    create_dir_all(result).expect("unable to create output directory for the tests");
    result
});

fn disassemble_test(name: &str) {
    let input = format!("res/computer_enhance/part1/{}", name);
    let output = format!("{}/{}", *OUTPUT_DIR, name);
    dbg!(&output);
    let expected = format!("{}.asm", input);

    disassemble(&input, &output).unwrap();

    let actual = read_to_string_and_remove_fluff(&output);
    let expected = read_to_string_and_remove_fluff(&expected);
    assert_eq!(expected, actual);
}

#[test]
fn listing_0037_single_register_mov() {
    disassemble_test("listing_0037_single_register_mov");
}

#[test]
fn listing_0038_many_register_mov() {
    disassemble_test("listing_0038_many_register_mov");
}

// #[test]
// fn listing_0039_more_movs() {
//     disassemble_test("listing_0039_more_movs");
// }
