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
    let result = "out/disassembly";
    create_dir_all(result).expect("unable to create output directory for the tests");
    result
});

fn disassemble_test(name: &str) {
    let input = format!("res/{}", name);
    let output = format!("{}/{}.asm", *OUTPUT_DIR, name);
    let expected = format!("{}.asm", input);

    disassemble(&input, &output).unwrap();

    let actual = read_to_string_and_remove_fluff(&output);
    let expected = read_to_string_and_remove_fluff(&expected);
    assert_eq!(expected, actual);
}

#[test]
fn mov_all_kinds_of_displacements() {
    disassemble_test("mov_all_kinds_of_displacements");
}

#[test]
fn mov_immediate_accumulator() {
    disassemble_test("mov_immediate_accumulator");
}
