use anyhow::Context;
use bitpatterns::bitpattern;
use bitpatterns::is_bit_match;
use instruction::{Instruction, Operation};
use std::{
    fmt::Display,
    fs::{read, write},
};
use thiserror::Error;

enum Literal {
    Byte(i8),
    Word(i16),
}
impl Literal {
    fn to_string(&self, be_explicit: bool) -> String {
        match (self, be_explicit) {
            (Literal::Byte(value), true) => format!("byte {}", value),
            (Literal::Byte(value), false) => format!("{}", value),
            (Literal::Word(value), true) => format!("word {}", value),
            (Literal::Word(value), false) => format!("{}", value),
        }
    }
}

enum Place {
    Register(Register),
    Immediate(Literal),
    Memory(Option<(Register, Option<Register>)>, i16),
}

impl Place {
    fn to_string(&self, be_explicit: bool) -> String {
        fn format_on_rhs(displacement: &i16) -> String {
            match displacement {
                ..0 => format!(" - {}", -displacement),
                0 => String::new(),
                1.. => format!(" + {}", displacement),
            }
        }

        match self {
            Place::Register(reg) => format!("{}", reg),
            Place::Immediate(value) => value.to_string(be_explicit),
            Place::Memory(None, displacement) => format!("[{}]", displacement),
            Place::Memory(Some((register, None)), displacement) => {
                format!("[{}{}]", register, format_on_rhs(displacement))
            }
            Place::Memory(Some((reg_a, Some(reg_b))), displacement) => {
                format!("[{} + {}{}]", reg_a, reg_b, format_on_rhs(displacement))
            }
        }
    }
}

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

impl Place {
    fn from_reg(reg: [bool; 3], data_type: DataType) -> Self {
        Place::Register(Register::from(reg, data_type))
    }
}

impl Register {
    fn accumulator(data_type: DataType) -> Self {
        match data_type {
            DataType::Byte => Self::Al,
            DataType::Word => Self::Ax,
        }
    }

    fn from(reg: [bool; 3], data_type: DataType) -> Self {
        match (reg, data_type) {
            ([false, false, false], DataType::Byte) => Register::Al,
            ([false, false, false], DataType::Word) => Register::Ax,
            ([false, false, true], DataType::Byte) => Register::Cl,
            ([false, false, true], DataType::Word) => Register::Cx,
            ([false, true, false], DataType::Byte) => Register::Dl,
            ([false, true, false], DataType::Word) => Register::Dx,
            ([false, true, true], DataType::Byte) => Register::Bl,
            ([false, true, true], DataType::Word) => Register::Bx,
            ([true, false, false], DataType::Byte) => Register::Ah,
            ([true, false, false], DataType::Word) => Register::Sp,
            ([true, false, true], DataType::Byte) => Register::Ch,
            ([true, false, true], DataType::Word) => Register::Bp,
            ([true, true, false], DataType::Byte) => Register::Dh,
            ([true, true, false], DataType::Word) => Register::Si,
            ([true, true, true], DataType::Byte) => Register::Bh,
            ([true, true, true], DataType::Word) => Register::Di,
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

    use super::Place;

    impl Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let need_to_be_explicit = !(matches!(self.destination, Place::Register(_))
                || matches!(self.source, Place::Register(_)));
            write!(
                f,
                "{} {}, {}",
                self.operation,
                self.destination.to_string(need_to_be_explicit),
                self.source.to_string(need_to_be_explicit)
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
        destination: Place,
        source: Place,
    }

    impl Instruction {
        pub fn new(operation: Operation, destination: Place, source: Place) -> Self {
            Instruction {
                operation,
                destination,
                source,
            }
        }
    }

    pub enum Operation {
        Mov,
    }
}

struct InstructionIterator<'a, I: Iterator<Item = &'a u8>> {
    byte_iterator: I,
}

fn is_bit_set(byte: u8, bit_id: usize) -> bool {
    let mask = 1 << (7 - bit_id);
    mask & byte != 0
}

fn r_or_m_to_registers_for_effective_address_calculation(
    r_or_m: [bool; 3],
    mode: [bool; 2],
) -> Option<(Register, Option<Register>)> {
    match (r_or_m, mode) {
        ([false, false, false], _) => Some((Register::Bx, Some(Register::Si))),
        ([false, false, true], _) => Some((Register::Bx, Some(Register::Di))),
        ([false, true, false], _) => Some((Register::Bp, Some(Register::Si))),
        ([false, true, true], _) => Some((Register::Bp, Some(Register::Di))),
        ([true, false, false], _) => Some((Register::Si, None)),
        ([true, false, true], _) => Some((Register::Di, None)),
        ([true, true, false], [false, false]) => None,
        ([true, true, false], _) => Some((Register::Bp, None)),
        ([true, true, true], _) => Some((Register::Bx, None)),
    }
}

fn bit_slice<const N: usize>(byte: u8, start: usize) -> [bool; N] {
    const { assert!(N <= 8, "a byte has less bits") };

    let mut result = [false; N];
    for (id, result_elem) in result.iter_mut().enumerate() {
        *result_elem = is_bit_set(byte, start + id);
    }
    result
}

#[derive(Clone, Copy)]
enum DataType {
    Byte,
    Word,
}

enum Mode {
    Memory { displacement: Option<DataType> },
    Register,
}

impl Mode {
    fn memory(displacement: Option<DataType>) -> Self {
        Mode::Memory { displacement }
    }

    fn from(bits: [bool; 2], r_or_m: [bool; 3]) -> Self {
        match (bits, r_or_m) {
            ([true, true], _) => Self::Register,
            ([true, false], _) | ([false, false], [true, true, false]) => {
                Self::memory(Some(DataType::Word))
            }
            ([false, true], _) => Self::memory(Some(DataType::Byte)),
            ([false, false], _) => Self::memory(None),
        }
    }
}

#[derive(Error, Debug)]
enum MachineCodeError {
    #[error("unexpected end of machine code")]
    UnexpectedEnd,
    #[error("unexpected pattern: '{0:08b}'")]
    UnexpectedPattern(u8),
}

impl<'a, I: Iterator<Item = &'a u8>> InstructionIterator<'a, I> {
    fn get_data_type(&self, byte: u8, bit_id: usize) -> DataType {
        if is_bit_set(byte, bit_id) {
            DataType::Word
        } else {
            DataType::Byte
        }
    }

    fn calculate_place(
        &mut self,
        mode: [bool; 2],
        r_or_m: [bool; 3],
        data_type: DataType,
    ) -> Result<Place, MachineCodeError> {
        match Mode::from(mode, r_or_m) {
            Mode::Memory { displacement } => {
                let displacement = match displacement {
                    None => 0,
                    Some(data_type) => self.expect_data(data_type)?,
                };

                let registers = r_or_m_to_registers_for_effective_address_calculation(r_or_m, mode);

                Ok(Place::Memory(registers, displacement))
            }
            Mode::Register => Ok(Place::Register(Register::from(r_or_m, data_type))),
        }
    }

    fn next_mov_between_r_or_m_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next()?;

        let is_reg_destination = is_bit_set(first_byte, 6);
        let data_type = self.get_data_type(first_byte, 7);

        let mode = bit_slice::<2>(second_byte, 0);
        let reg = bit_slice::<3>(second_byte, 2);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let reg = Place::from_reg(reg, data_type);
        let r_or_m = self.calculate_place(mode, r_or_m, data_type)?;

        let (source, destination) = if is_reg_destination {
            (r_or_m, reg)
        } else {
            (reg, r_or_m)
        };

        Ok(Instruction::new(Operation::Mov, destination, source))
    }

    fn next_mov_immediate_to_r_m(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next()?;
        if !is_bit_match!("..000...", second_byte) {
            return Err(MachineCodeError::UnexpectedPattern(second_byte));
        }

        let data_type = self.get_data_type(first_byte, 7);

        let mode = bit_slice::<2>(second_byte, 0);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let destination = self.calculate_place(mode, r_or_m, data_type)?;
        let source = Place::Immediate(self.expect_literal(data_type)?);

        Ok(Instruction::new(Operation::Mov, destination, source))
    }

    fn next_mov_immediate_to_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let data_type = self.get_data_type(first_byte, 4);
        let reg = bit_slice::<3>(first_byte, 5);

        let destination = Place::from_reg(reg, data_type);
        let source = Place::Immediate(self.expect_literal(data_type)?);
        Ok(Instruction::new(Operation::Mov, destination, source))
    }

    fn next_mov_between_acc_memory(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let is_memory_destination = is_bit_set(first_byte, 6);

        let data_type = self.get_data_type(first_byte, 7);
        let acc = Place::Register(Register::accumulator(data_type));
        let memory = Place::Memory(None, self.expect_data(DataType::Word)?);

        let (destination, source) = if is_memory_destination {
            (memory, acc)
        } else {
            (acc, memory)
        };

        Ok(Instruction::new(Operation::Mov, destination, source))
    }

    fn expect_next(&mut self) -> Result<u8, MachineCodeError> {
        self.byte_iterator
            .next()
            .copied()
            .ok_or(MachineCodeError::UnexpectedEnd)
    }

    fn expect_literal(&mut self, data_type: DataType) -> Result<Literal, MachineCodeError> {
        let low = self.expect_next()?;

        match data_type {
            DataType::Byte => Ok(Literal::Byte(low as i8)),
            DataType::Word => {
                let high = self.expect_next()? as i16;
                Ok(Literal::Word(low as i16 | (high << 8)))
            }
        }
    }

    fn next_impl(&mut self, first_byte: u8) -> Result<Instruction, MachineCodeError> {
        if is_bit_match!("100010..", first_byte) {
            return self.next_mov_between_r_or_m_reg(first_byte);
        };

        if is_bit_match!("1100011.", first_byte) {
            return self.next_mov_immediate_to_r_m(first_byte);
        };

        if is_bit_match!("1011....", first_byte) {
            return self.next_mov_immediate_to_reg(first_byte);
        };

        if is_bit_match!("101000..", first_byte) {
            return self.next_mov_between_acc_memory(first_byte);
        };

        Err(MachineCodeError::UnexpectedPattern(first_byte))
    }

    fn expect_data(&mut self, data_type: DataType) -> Result<i16, MachineCodeError> {
        match self.expect_literal(data_type)? {
            Literal::Byte(value) => Ok(value as i16),
            Literal::Word(value) => Ok(value),
        }
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
