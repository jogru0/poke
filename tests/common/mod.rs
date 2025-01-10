use poke::intel_8086::disassemble;
use pretty_assertions::assert_eq;
use std::{
    fs::{read, read_to_string},
    process::Command,
};

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

pub fn disassemble_test(name: &str, test_dir: &str, expect_to_reproduce_initial_asm: bool) {
    let initial_machine_code = format!("res/{}/{}", test_dir, name);
    let nasm_machine_code_from_disassembled = format!("out/{}/{}", test_dir, name);
    let disassembled = format!("{}.asm", nasm_machine_code_from_disassembled);
    let second_disassembled = format!("{}_second.asm", nasm_machine_code_from_disassembled);

    disassemble(&initial_machine_code, &disassembled).unwrap();

    let nasm_output = Command::new("nasm").arg(&disassembled).output().unwrap();
    assert!(nasm_output.status.success());

    disassemble(&nasm_machine_code_from_disassembled, &second_disassembled).unwrap();

    let expected = read_to_string(&disassembled).unwrap();
    let actual = read_to_string(&second_disassembled).unwrap();
    assert_eq!(expected, actual);

    let expected = read(&initial_machine_code).unwrap();
    let actual = read(&nasm_machine_code_from_disassembled).unwrap();
    assert_eq!(expected, actual);

    if expect_to_reproduce_initial_asm {
        let inital_asm = format!("{}.asm", initial_machine_code);
        let expected = read_to_string_and_remove_fluff(&inital_asm);
        let actual = read_to_string_and_remove_fluff(&disassembled);
        assert_eq!(expected, actual);
    }
}
