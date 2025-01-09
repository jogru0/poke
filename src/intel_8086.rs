use anyhow::Context;
use instruction::Instruction;
use std::{
    fmt::Display,
    fs::{read, write},
};
use thiserror::Error;

enum Register {
    Al,
    Ax,
    Cl,
    Cx,
    Dl,
    Dx,
    Bl,
    Bx,
    Ah,
    Sp,
    Ch,
    Bp,
    Dh,
    Si,
    Bh,
    Di,
}

impl Register {
    fn from(reg: [bool; 3], is_wide: bool) -> Self {
        match (reg, is_wide) {
            ([false, false, false], false) => Register::Al,
            ([false, false, false], true) => Register::Ax,
            ([false, false, true], false) => Register::Cl,
            ([false, false, true], true) => Register::Cx,
            ([false, true, false], false) => Register::Dl,
            ([false, true, false], true) => Register::Dx,
            ([false, true, true], false) => Register::Bl,
            ([false, true, true], true) => Register::Bx,
            ([true, false, false], false) => Register::Ah,
            ([true, false, false], true) => Register::Sp,
            ([true, false, true], false) => Register::Ch,
            ([true, false, true], true) => Register::Bp,
            ([true, true, false], false) => Register::Dh,
            ([true, true, false], true) => Register::Si,
            ([true, true, true], false) => Register::Bh,
            ([true, true, true], true) => Register::Di,
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Ax => write!(f, "ax"),
            Register::Bx => write!(f, "bx"),
            Register::Cx => write!(f, "cx"),
            Register::Al => write!(f, "al"),
            Register::Cl => write!(f, "cl"),
            Register::Dl => write!(f, "dl"),
            Register::Dx => write!(f, "dx"),
            Register::Bl => write!(f, "bl"),
            Register::Ah => write!(f, "ah"),
            Register::Sp => write!(f, "sp"),
            Register::Ch => write!(f, "ch"),
            Register::Bp => write!(f, "bp"),
            Register::Dh => write!(f, "dh"),
            Register::Si => write!(f, "si"),
            Register::Bh => write!(f, "bh"),
            Register::Di => write!(f, "di"),
        }
    }
}

mod instruction {

    use std::fmt::Display;

    use super::Register;

    impl Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{} {}, {}",
                self.operation, self.destination, self.source
            )
        }
    }

    impl Display for Operation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Operation::Mov => write!(f, "mov"),
            }
        }
    }

    pub struct Instruction {
        operation: Operation,
        // reg_id_destination: bool,
        // mod: u8,
        // reg: u8,
        // rm: u8,
        source: Register,
        destination: Register,
    }

    impl Instruction {
        pub fn new(operation: Operation, source: Register, destination: Register) -> Self {
            Instruction {
                operation,
                source,
                destination,
            }
        }
    }

    pub enum Operation {
        Mov,
    }

    impl From<[bool; 6]> for Operation {
        fn from(value: [bool; 6]) -> Self {
            match value {
                [true, false, false, false, true, false] => Operation::Mov,
                _ => unimplemented!("{value:?}"),
            }
        }
    }
}

struct InstructionIterator<'a, I: Iterator<Item = &'a u8>> {
    byte_iterator: I,
}

fn is_bit_set(byte: u8, bit_id: usize) -> bool {
    let mask = 1 << (7 - bit_id);
    mask & byte != 0
}

fn bit_slice<const N: usize>(byte: u8, start: usize) -> [bool; N] {
    const { assert!(N <= 8, "a byte has less bits") };

    let mut result = [false; N];
    for (id, result_elem) in result.iter_mut().enumerate() {
        *result_elem = is_bit_set(byte, start + id);
    }
    result
}

enum Displacement {
    Zero,
    Eight,
    Sixteen,
}
enum Mode {
    Memory(#[expect(dead_code)] Displacement),
    Register,
}

impl Mode {
    fn from(bits: [bool; 2], r_or_m: [bool; 3]) -> Self {
        match (bits, r_or_m) {
            ([true, true], _) => Self::Register,
            ([true, false], _) | ([false, false], [true, true, false]) => {
                Mode::Memory(Displacement::Sixteen)
            }
            ([false, true], _) => Mode::Memory(Displacement::Eight),
            ([false, false], _) => Mode::Memory(Displacement::Zero),
        }
    }
}

#[derive(Error, Debug)]
enum MachineCodeError {
    #[error("unexpected end of machine code")]
    UnexpectedEnd,
}

impl<'a, I: Iterator<Item = &'a u8>> InstructionIterator<'a, I> {
    fn next_impl(&mut self, first_byte: u8) -> Result<Instruction, MachineCodeError> {
        let Some(&second_byte) = self.byte_iterator.next() else {
            return Err(MachineCodeError::UnexpectedEnd);
        };

        let operation = bit_slice::<6>(first_byte, 0).into();
        let is_reg_destination = is_bit_set(first_byte, 6);
        let is_wide = is_bit_set(first_byte, 7);

        let mode = bit_slice::<2>(second_byte, 0);
        let reg = bit_slice::<3>(second_byte, 2);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let mode = Mode::from(mode, r_or_m);
        assert!(matches!(mode, Mode::Register));

        let reg = Register::from(reg, is_wide);
        let reg2 = Register::from(r_or_m, is_wide);

        let (source, destination) = if is_reg_destination {
            (reg2, reg)
        } else {
            (reg, reg2)
        };

        Ok(Instruction::new(operation, source, destination))
    }
}

impl<'a, I: Iterator<Item = &'a u8>> Iterator for InstructionIterator<'a, I> {
    type Item = Result<Instruction, MachineCodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.byte_iterator
            .next()
            .map(|first_byte| self.next_impl(*first_byte))
    }
}

pub fn disassemble(input_file: &str, output_file: &str) -> Result<(), anyhow::Error> {
    let machine_code =
        read(input_file).with_context(|| format!("could not read file {input_file:?}"))?;

    let instuction_iterator = InstructionIterator {
        byte_iterator: machine_code.iter(),
    };

    let instructions: Result<Vec<_>, _> = instuction_iterator.collect();
    let instructions = instructions?;

    let mut disassembly = "bits 16\n".to_string();
    for instruction in instructions {
        disassembly += &format!("{}\n", instruction);
    }

    write(output_file, disassembly)
        .with_context(|| format!("could not write file {output_file:?}"))?;

    Ok(())
}
