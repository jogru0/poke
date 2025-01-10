use anyhow::Context;
use bitpatterns::bitpattern;
use bitpatterns::is_bit_match;
use instruction::BinaryOperation;
use instruction::Instruction;
use instruction::JumpOperation;
use std::{
    fmt::Display,
    fs::{read, write},
};
use thiserror::Error;

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

    impl Display for JumpInstruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            // The jump is relative to the next instruction, so in the assembly,
            // we have to add the two bytes this instruction takes.
            let asm_offset = self.offset + 2;
            if asm_offset < 0 {
                write!(f, "{} ${}", self.operation, asm_offset)
            } else {
                write!(f, "{} $+{}", self.operation, asm_offset)
            }
        }
    }

    impl Display for BinaryInstruction {
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

    impl Display for BinaryOperation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                BinaryOperation::Mov => write!(f, "mov"),
                BinaryOperation::Add => write!(f, "add"),
                BinaryOperation::Sub => write!(f, "sub"),
                BinaryOperation::Cmp => write!(f, "cmp"),
            }
        }
    }

    #[derive(Debug)]
    pub struct BinaryInstruction {
        operation: BinaryOperation,
        destination: Place,
        source: Place,
    }

    #[derive(Debug)]
    pub struct JumpInstruction {
        operation: JumpOperation,
        offset: i8,
    }

    #[derive(Debug)]
    pub enum Instruction {
        Binary(BinaryInstruction),
        Jump(JumpInstruction),
    }

    impl Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Instruction::Binary(binary) => write!(f, "{}", binary),
                Instruction::Jump(jump) => write!(f, "{}", jump),
            }
        }
    }

    impl Instruction {
        pub fn new_binary(operation: BinaryOperation, destination: Place, source: Place) -> Self {
            Instruction::Binary(BinaryInstruction {
                operation,
                destination,
                source,
            })
        }

        pub fn new_jump(operation: JumpOperation, offset: i8) -> Self {
            Instruction::Jump(JumpInstruction { operation, offset })
        }
    }

    #[derive(Debug)]
    pub enum JumpOperation {
        Je,
        Jl,
        Jle,
        Jb,
        Jbe,
        Jp,
        Jo,
        Js,
        Jne,
        Jnl,
        Jnle,
        Jnb,
        Jnbe,
        Jnp,
        Jno,
        Jns,
        Loop,
        Loopz,
        Loopnz,
        Jcxz,
    }

    impl Display for JumpOperation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                JumpOperation::Je => write!(f, "je"),
                JumpOperation::Jl => write!(f, "jl"),
                JumpOperation::Jle => write!(f, "jle"),
                JumpOperation::Jb => write!(f, "jb"),
                JumpOperation::Jbe => write!(f, "jbe"),
                JumpOperation::Jp => write!(f, "jp"),
                JumpOperation::Jo => write!(f, "jo"),
                JumpOperation::Js => write!(f, "js"),
                JumpOperation::Jne => write!(f, "jne"),
                JumpOperation::Jnl => write!(f, "jnl"),
                JumpOperation::Jnle => write!(f, "jnle"),
                JumpOperation::Jnb => write!(f, "jnb"),
                JumpOperation::Jnbe => write!(f, "jnbe"),
                JumpOperation::Jnp => write!(f, "jnp"),
                JumpOperation::Jno => write!(f, "jno"),
                JumpOperation::Jns => write!(f, "jns"),
                JumpOperation::Loop => write!(f, "loop"),
                JumpOperation::Loopz => write!(f, "loopz"),
                JumpOperation::Loopnz => write!(f, "loopnz"),
                JumpOperation::Jcxz => write!(f, "jcxz"),
            }
        }
    }

    impl JumpOperation {
        pub fn from(bits: u8) -> Self {
            match bits {
                0b01110100 => JumpOperation::Je,
                0b01111100 => JumpOperation::Jl,
                0b01111110 => JumpOperation::Jle,
                0b01110010 => JumpOperation::Jb,
                0b01110110 => JumpOperation::Jbe,
                0b01111010 => JumpOperation::Jp,
                0b01110000 => JumpOperation::Jo,
                0b01111000 => JumpOperation::Js,
                0b01110101 => JumpOperation::Jne,
                0b01111101 => JumpOperation::Jnl,
                0b01111111 => JumpOperation::Jnle,
                0b01110011 => JumpOperation::Jnb,
                0b01110111 => JumpOperation::Jnbe,
                0b01111011 => JumpOperation::Jnp,
                0b01110001 => JumpOperation::Jno,
                0b01111001 => JumpOperation::Jns,
                0b11100010 => JumpOperation::Loop,
                0b11100001 => JumpOperation::Loopz,
                0b11100000 => JumpOperation::Loopnz,
                0b11100011 => JumpOperation::Jcxz,

                _ => unimplemented!("missing jump operation associated to {bits:08b}"),
            }
        }
    }

    #[derive(Debug)]
    pub enum BinaryOperation {
        Mov,
        Add,
        Sub,
        Cmp,
    }

    impl BinaryOperation {
        pub fn from(bits: [bool; 3]) -> Self {
            match bits {
                [false, false, false] => BinaryOperation::Add,
                [true, false, true] => BinaryOperation::Sub,
                [true, true, true] => BinaryOperation::Cmp,
                _ => unimplemented!("missing binary operation associated to {bits:?}"),
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
                    Some(data_type) => self.expect_i16(data_type)?,
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

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            source,
        ))
    }

    fn next_op_between_r_or_m_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let op = BinaryOperation::from(bit_slice::<3>(first_byte, 2));

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

        Ok(Instruction::new_binary(op, destination, source))
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
        let source = Place::Immediate(self.expect_literal(data_type, false)?);

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            source,
        ))
    }

    fn next_op_immediate_to_r_m(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next()?;

        let do_sign_extension_instead_of_expecting_high_bits = is_bit_set(first_byte, 6);
        let data_type = self.get_data_type(first_byte, 7);
        let op = BinaryOperation::from(bit_slice::<3>(second_byte, 2));
        let mode = bit_slice::<2>(second_byte, 0);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let destination = self.calculate_place(mode, r_or_m, data_type)?;
        let source = Place::Immediate(
            self.expect_literal(data_type, do_sign_extension_instead_of_expecting_high_bits)?,
        );

        Ok(Instruction::new_binary(op, destination, source))
    }

    fn next_mov_immediate_to_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let data_type = self.get_data_type(first_byte, 4);
        let reg = bit_slice::<3>(first_byte, 5);

        let destination = Place::from_reg(reg, data_type);
        let source = Place::Immediate(self.expect_literal(data_type, false)?);
        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            source,
        ))
    }

    fn next_mov_between_acc_memory(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let is_memory_destination = is_bit_set(first_byte, 6);

        let data_type = self.get_data_type(first_byte, 7);
        let acc = Place::Register(Register::accumulator(data_type));
        let memory = Place::Memory(None, self.expect_i16(DataType::Word)?);

        let (destination, source) = if is_memory_destination {
            (memory, acc)
        } else {
            (acc, memory)
        };

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            source,
        ))
    }

    fn next_op_immediate_to_acc(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let op = BinaryOperation::from(bit_slice::<3>(first_byte, 2));
        let data_type = self.get_data_type(first_byte, 7);

        let acc = Place::Register(Register::accumulator(data_type));
        let immediate = Place::Immediate(self.expect_literal(data_type, false)?);

        Ok(Instruction::new_binary(op, acc, immediate))
    }

    fn expect_next(&mut self) -> Result<u8, MachineCodeError> {
        self.byte_iterator
            .next()
            .copied()
            .ok_or(MachineCodeError::UnexpectedEnd)
    }

    fn expect_literal(
        &mut self,
        data_type: DataType,
        do_sign_extension_instead_of_expecting_high_bits: bool,
    ) -> Result<Literal, MachineCodeError> {
        let low = self.expect_next()?;

        match data_type {
            DataType::Byte => Ok(Literal::Byte(low as i8)),
            DataType::Word => {
                let word = if do_sign_extension_instead_of_expecting_high_bits {
                    low as i16
                } else {
                    let high = self.expect_next()? as i16;
                    low as i16 | (high << 8)
                };
                Ok(Literal::Word(word))
            }
        }
    }

    fn next_op_jump(&mut self, first_byte: u8) -> Result<Instruction, MachineCodeError> {
        let op = JumpOperation::from(first_byte);
        let offset = self.expect_i8()?;
        Ok(Instruction::new_jump(op, offset))
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

        if is_bit_match!("00...0..", first_byte) {
            return self.next_op_between_r_or_m_reg(first_byte);
        };

        if is_bit_match!("100000..", first_byte) {
            return self.next_op_immediate_to_r_m(first_byte);
        };

        if is_bit_match!("00...10.", first_byte) {
            return self.next_op_immediate_to_acc(first_byte);
        };

        self.next_op_jump(first_byte)
    }

    fn expect_i16(&mut self, data_type: DataType) -> Result<i16, MachineCodeError> {
        match self.expect_literal(data_type, false)? {
            Literal::Byte(value) => Ok(value as i16),
            Literal::Word(value) => Ok(value),
        }
    }

    fn expect_i8(&mut self) -> Result<i8, MachineCodeError> {
        Ok(self.expect_next()? as i8)
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
