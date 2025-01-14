use anyhow::Context;
use bitpatterns::bitpattern;
use bitpatterns::is_bit_match;
use instruction::binary_instruction::BinaryInstruction;
use instruction::BinaryOperation;
use instruction::Instruction;
use instruction::JumpInstruction;
use instruction::JumpOperation;
use std::path::Path;
use std::{
    fmt::Display,
    fs::{read, write},
};
use thiserror::Error;

#[derive(Debug, Clone, Copy)]
struct MemoryPlace {
    registers: Option<(Register, Option<Register>)>,
    displacement: i16,
}

#[derive(Debug, Clone, Copy)]
enum Place {
    Register(Register),
    Memory(MemoryPlace),
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Place(Place),
    Immediate(i16),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Place(place) => write!(f, "{place}"),
            &Self::Immediate(value) => {
                if (-128..128).contains(&value) {
                    write!(f, "{}", value)
                } else {
                    write!(f, "{}", value as u16)
                }
            }
        }
    }
}

impl Display for MemoryPlace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn format_on_rhs(displacement: &i16) -> String {
            match displacement {
                ..0 => format!("-{}", -displacement),
                0 => String::new(),
                1.. => format!("+{}", displacement),
            }
        }

        match self {
            Self {
                registers: None,
                displacement,
            } => write!(f, "{}", format_on_rhs(displacement)),
            Self {
                registers: Some((register, None)),
                displacement,
            } => write!(f, "{}{}", register, format_on_rhs(displacement)),
            Self {
                registers: Some((reg_a, Some(reg_b))),
                displacement,
            } => write!(f, "{}+{}{}", reg_a, reg_b, format_on_rhs(displacement)),
        }
    }
}

impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(reg) => write!(f, "{}", reg),
            Self::Memory(memory_place) => write!(f, "[{}]", memory_place),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Register {
    id: usize,
    data_type: DataType,
}

impl Register {
    const AX: Self = Self {
        id: 0,
        data_type: DataType::Word,
    };

    const CX: Self = Self {
        id: 2,
        data_type: DataType::Word,
    };

    const DX: Self = Self {
        id: 3,
        data_type: DataType::Word,
    };
    const BX: Self = Self {
        id: 1,
        data_type: DataType::Word,
    };

    const SP: Self = Self {
        id: 4,
        data_type: DataType::Word,
    };
    const BP: Self = Self {
        id: 5,
        data_type: DataType::Word,
    };
    const SI: Self = Self {
        id: 6,
        data_type: DataType::Word,
    };
    const DI: Self = Self {
        id: 7,
        data_type: DataType::Word,
    };
    const AL: Self = Self {
        id: 1,
        data_type: DataType::Byte,
    };

    const CL: Self = Self {
        id: 5,
        data_type: DataType::Byte,
    };

    const DL: Self = Self {
        id: 7,
        data_type: DataType::Byte,
    };

    const BL: Self = Self {
        id: 3,
        data_type: DataType::Byte,
    };

    const AH: Self = Self {
        id: 0,
        data_type: DataType::Byte,
    };

    const CH: Self = Self {
        id: 4,
        data_type: DataType::Byte,
    };

    const DH: Self = Self {
        id: 6,
        data_type: DataType::Byte,
    };

    const BH: Self = Self {
        id: 2,
        data_type: DataType::Byte,
    };

    const CS: Self = Self {
        id: 9,
        data_type: DataType::Word,
    };

    const DS: Self = Self {
        id: 11,
        data_type: DataType::Word,
    };

    const ES: Self = Self {
        id: 8,
        data_type: DataType::Word,
    };

    const SS: Self = Self {
        id: 10,
        data_type: DataType::Word,
    };

    fn accumulator(data_type: DataType) -> Register {
        match data_type {
            DataType::Byte => Register::AL,
            DataType::Word => Register::AX,
        }
    }
}

impl Register {
    fn from_reg(reg: [bool; 3], data_type: DataType) -> Self {
        match (reg, data_type) {
            ([false, false, false], DataType::Byte) => Register::AL,
            ([false, false, true], DataType::Byte) => Register::CL,
            ([false, true, false], DataType::Byte) => Register::DL,
            ([false, true, true], DataType::Byte) => Register::BL,
            ([true, false, false], DataType::Byte) => Register::AH,
            ([true, false, true], DataType::Byte) => Register::CH,
            ([true, true, false], DataType::Byte) => Register::DH,
            ([true, true, true], DataType::Byte) => Register::BH,
            ([false, false, false], DataType::Word) => Register::AX,
            ([false, false, true], DataType::Word) => Register::CX,
            ([false, true, false], DataType::Word) => Register::DX,
            ([false, true, true], DataType::Word) => Register::BX,
            ([true, false, false], DataType::Word) => Register::SP,
            ([true, false, true], DataType::Word) => Register::BP,
            ([true, true, false], DataType::Word) => Register::SI,
            ([true, true, true], DataType::Word) => Register::DI,
        }
    }

    fn from_sr(sr: [bool; 2]) -> Self {
        match sr {
            [false, false] => Register::ES,
            [false, true] => Register::CS,
            [true, false] => Register::SS,
            [true, true] => Register::DS,
        }
    }
}

static BYTE_REGISTER_NAMES: [&str; 8] = ["ah", "al", "bh", "bl", "ch", "cl", "dh", "dl"];
static WORD_REGISTER_NAMES: [&str; 12] = [
    "ax", "bx", "cx", "dx", "sp", "bp", "si", "di", "es", "cs", "ss", "ds",
];

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data_type {
            DataType::Byte => write!(f, "{}", BYTE_REGISTER_NAMES[self.id]),
            DataType::Word => write!(f, "{}", WORD_REGISTER_NAMES[self.id]),
        }
    }
}

mod instruction {

    use std::fmt::Display;

    use binary_instruction::BinaryInstruction;

    use super::{DataType, Place, Value};

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

    pub mod binary_instruction {
        use std::fmt::Display;

        use crate::i8086::{DataType, Place, Value};

        use super::BinaryOperation;

        #[derive(Debug, Clone, Copy)]
        pub struct BinaryInstruction {
            operation: BinaryOperation,
            destination: Place,
            source: Value,
            data_type: DataType,
        }

        impl Display for BinaryInstruction {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let need_to_be_explicit = !(matches!(self.destination, Place::Register(_))
                    || matches!(self.source, Value::Place(Place::Register(_))))
                    || matches!(self.destination, Place::Memory(_));

                let explicit_data_type = if need_to_be_explicit {
                    match self.data_type {
                        DataType::Byte => "byte ",
                        DataType::Word => "word ",
                    }
                } else {
                    ""
                };
                write!(
                    f,
                    "{} {}{}, {}",
                    self.operation, explicit_data_type, self.destination, self.source
                )
            }
        }

        impl BinaryInstruction {
            pub fn data_type(&self) -> DataType {
                self.data_type
            }

            pub fn new(
                operation: BinaryOperation,
                destination: Place,
                source: Value,
                data_type: DataType,
            ) -> Self {
                assert!(
                    !matches!(destination, Place::Memory(_))
                        || !matches!(source, Value::Place(Place::Memory(_)))
                );

                assert!(match destination {
                    Place::Register(reg) => reg.data_type == data_type,
                    _ => true,
                });

                assert!(match source {
                    Value::Place(Place::Register(reg)) => reg.data_type == data_type,
                    _ => true,
                });

                Self {
                    operation,
                    destination,
                    source,
                    data_type,
                }
            }

            pub(crate) fn destination(&self) -> Place {
                self.destination
            }

            pub(crate) fn source(&self) -> Value {
                self.source
            }

            pub(crate) fn operation(&self) -> BinaryOperation {
                self.operation
            }
        }
    }

    #[derive(Debug)]
    pub struct JumpInstruction {
        operation: JumpOperation,
        offset: i16,
    }

    impl JumpInstruction {
        pub fn operation(&self) -> JumpOperation {
            self.operation
        }

        pub fn offset(&self) -> i16 {
            self.offset
        }
    }

    #[derive(Debug)]
    pub enum Instruction {
        Binary(BinaryInstruction),
        Jump(JumpInstruction),
        Halt,
    }

    impl Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Instruction::Binary(binary) => write!(f, "{}", binary),
                Instruction::Jump(jump) => write!(f, "{}", jump),
                Instruction::Halt => write!(f, "hlt"),
            }
        }
    }

    impl Instruction {
        pub fn new_binary(
            operation: BinaryOperation,
            destination: Place,
            source: Value,
            data_type: DataType,
        ) -> Self {
            Instruction::Binary(BinaryInstruction::new(
                operation,
                destination,
                source,
                data_type,
            ))
        }

        pub fn new_jump(operation: JumpOperation, offset: i8) -> Self {
            Instruction::Jump(JumpInstruction {
                operation,
                offset: offset as i16,
            })
        }
    }

    #[derive(Debug, Clone, Copy)]
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

    #[derive(Debug, Clone, Copy)]
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

struct MachineCodeReader<'a> {
    machine_code: &'a [u8],
    instruction_pointer: usize,
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
        ([false, false, false], _) => Some((Register::BX, Some(Register::SI))),
        ([false, false, true], _) => Some((Register::BX, Some(Register::DI))),
        ([false, true, false], _) => Some((Register::BP, Some(Register::SI))),
        ([false, true, true], _) => Some((Register::BP, Some(Register::DI))),
        ([true, false, false], _) => Some((Register::SI, None)),
        ([true, false, true], _) => Some((Register::DI, None)),
        ([true, true, false], [false, false]) => None,
        ([true, true, false], _) => Some((Register::BP, None)),
        ([true, true, true], _) => Some((Register::BX, None)),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DataType {
    Byte,
    Word,
}

enum Mode {
    Memory {
        displacement_data_size: Option<DataType>,
    },
    Register,
}

impl Mode {
    fn memory(displacement_data_size: Option<DataType>) -> Self {
        Mode::Memory {
            displacement_data_size,
        }
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

impl<'a> MachineCodeReader<'a> {
    fn new(machine_code: &'a [u8]) -> Self {
        Self {
            machine_code,
            instruction_pointer: 0,
        }
    }

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
            Mode::Memory {
                displacement_data_size: displacement,
            } => {
                let displacement = match displacement {
                    None => 0,
                    Some(data_type) => self.expect_i16(data_type)?,
                };

                let registers = r_or_m_to_registers_for_effective_address_calculation(r_or_m, mode);

                Ok(Place::Memory(MemoryPlace {
                    registers,
                    displacement,
                }))
            }
            Mode::Register => Ok(Place::Register(Register::from_reg(r_or_m, data_type))),
        }
    }

    fn next_mov_between_r_or_m_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next_byte()?;

        let is_reg_destination = is_bit_set(first_byte, 6);
        let data_type = self.get_data_type(first_byte, 7);

        let mode = bit_slice::<2>(second_byte, 0);
        let reg = bit_slice::<3>(second_byte, 2);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let reg = Place::Register(Register::from_reg(reg, data_type));
        let r_or_m = self.calculate_place(mode, r_or_m, data_type)?;

        let (source, destination) = if is_reg_destination {
            (r_or_m, reg)
        } else {
            (reg, r_or_m)
        };

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            Value::Place(source),
            data_type,
        ))
    }

    fn next_mov_between_r_or_m_sr(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next_byte()?;
        if is_bit_set(second_byte, 2) {
            return Err(MachineCodeError::UnexpectedPattern(second_byte));
        }

        let is_sr_destination = is_bit_set(first_byte, 6);

        let mode = bit_slice::<2>(second_byte, 0);
        let sr = bit_slice::<2>(second_byte, 3);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let data_type = DataType::Word;

        let sr = Place::Register(Register::from_sr(sr));
        let r_or_m = self.calculate_place(mode, r_or_m, data_type)?;

        let (source, destination) = if is_sr_destination {
            (r_or_m, sr)
        } else {
            (sr, r_or_m)
        };

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            Value::Place(source),
            data_type,
        ))
    }

    fn next_op_between_r_or_m_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let op = BinaryOperation::from(bit_slice::<3>(first_byte, 2));

        let second_byte = self.expect_next_byte()?;

        let is_reg_destination = is_bit_set(first_byte, 6);
        let data_type = self.get_data_type(first_byte, 7);

        let mode = bit_slice::<2>(second_byte, 0);
        let reg = bit_slice::<3>(second_byte, 2);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let reg = Place::Register(Register::from_reg(reg, data_type));
        let r_or_m = self.calculate_place(mode, r_or_m, data_type)?;

        let (source, destination) = if is_reg_destination {
            (r_or_m, reg)
        } else {
            (reg, r_or_m)
        };

        Ok(Instruction::new_binary(
            op,
            destination,
            Value::Place(source),
            data_type,
        ))
    }

    fn next_mov_immediate_to_r_m(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next_byte()?;
        if !is_bit_match!("..000...", second_byte) {
            return Err(MachineCodeError::UnexpectedPattern(second_byte));
        }

        let data_type = self.get_data_type(first_byte, 7);

        let mode = bit_slice::<2>(second_byte, 0);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let destination = self.calculate_place(mode, r_or_m, data_type)?;
        let source = Value::Immediate(self.expect_literal(data_type, false)?);

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            source,
            data_type,
        ))
    }

    fn next_op_immediate_to_r_m(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let second_byte = self.expect_next_byte()?;

        let do_sign_extension_instead_of_expecting_high_bits = is_bit_set(first_byte, 6);
        let data_type = self.get_data_type(first_byte, 7);
        let op = BinaryOperation::from(bit_slice::<3>(second_byte, 2));
        let mode = bit_slice::<2>(second_byte, 0);
        let r_or_m = bit_slice::<3>(second_byte, 5);

        let destination = self.calculate_place(mode, r_or_m, data_type)?;
        let source = Value::Immediate(
            self.expect_literal(data_type, do_sign_extension_instead_of_expecting_high_bits)?,
        );

        Ok(Instruction::new_binary(op, destination, source, data_type))
    }

    fn next_mov_immediate_to_reg(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let data_type = self.get_data_type(first_byte, 4);
        let reg = bit_slice::<3>(first_byte, 5);

        let destination = Place::Register(Register::from_reg(reg, data_type));
        let source = Value::Immediate(self.expect_literal(data_type, false)?);
        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            source,
            data_type,
        ))
    }

    fn next_mov_between_acc_memory(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let is_memory_destination = is_bit_set(first_byte, 6);

        let data_type = self.get_data_type(first_byte, 7);
        let acc = Place::Register(Register::accumulator(data_type));
        let memory = Place::Memory(MemoryPlace {
            registers: None,
            displacement: self.expect_i16(DataType::Word)?,
        });

        let (destination, source) = if is_memory_destination {
            (memory, acc)
        } else {
            (acc, memory)
        };

        Ok(Instruction::new_binary(
            BinaryOperation::Mov,
            destination,
            Value::Place(source),
            data_type,
        ))
    }

    fn next_op_immediate_to_acc(
        &mut self,
        first_byte: u8,
    ) -> Result<Instruction, MachineCodeError> {
        let op = BinaryOperation::from(bit_slice::<3>(first_byte, 2));
        let data_type = self.get_data_type(first_byte, 7);

        let acc = Place::Register(Register::accumulator(data_type));
        let immediate = Value::Immediate(self.expect_literal(data_type, false)?);

        Ok(Instruction::new_binary(op, acc, immediate, data_type))
    }

    fn expect_next_byte(&mut self) -> Result<u8, MachineCodeError> {
        let Some(&res) = self.machine_code.get(self.instruction_pointer) else {
            return Err(MachineCodeError::UnexpectedEnd);
        };
        self.instruction_pointer += 1;
        Ok(res)
    }

    fn expect_literal(
        &mut self,
        data_type: DataType,
        do_sign_extension_instead_of_expecting_high_bits: bool,
    ) -> Result<i16, MachineCodeError> {
        let low = self.expect_next_byte()?;
        match data_type {
            DataType::Byte => Ok(low as i8 as i16),
            DataType::Word => {
                let word = if do_sign_extension_instead_of_expecting_high_bits {
                    low as i8 as i16
                } else {
                    let high = self.expect_next_byte()? as u16;
                    (low as u16 | (high << 8)) as i16
                };
                Ok(word)
            }
        }
    }

    fn next_op_jump(&mut self, first_byte: u8) -> Result<Instruction, MachineCodeError> {
        let op = JumpOperation::from(first_byte);
        let offset = self.expect_i8()?;
        Ok(Instruction::new_jump(op, offset))
    }

    fn next(&mut self) -> Result<Instruction, MachineCodeError> {
        let first_byte = self.expect_next_byte()?;

        if is_bit_match!("100010..", first_byte) {
            return self.next_mov_between_r_or_m_reg(first_byte);
        };

        if is_bit_match!("100011.0", first_byte) {
            return self.next_mov_between_r_or_m_sr(first_byte);
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

        if is_bit_match!("11110100", first_byte) {
            return Ok(Instruction::Halt);
        };

        self.next_op_jump(first_byte)
    }

    fn expect_i16(&mut self, data_type: DataType) -> Result<i16, MachineCodeError> {
        self.expect_literal(data_type, false)
    }

    fn expect_i8(&mut self) -> Result<i8, MachineCodeError> {
        Ok(self.expect_next_byte()? as i8)
    }

    fn is_done(&self) -> bool {
        self.machine_code.len() == self.instruction_pointer
    }
}

pub fn disassemble(input_file: &str, output_file: &str) -> Result<(), anyhow::Error> {
    let machine_code =
        read(input_file).with_context(|| format!("could not read file {input_file:?}"))?;

    let mut machine_code_reader = MachineCodeReader::new(&machine_code);

    let mut instructions = Vec::new();
    while !machine_code_reader.is_done() {
        instructions.push(machine_code_reader.next()?);
    }

    let mut disassembly = "bits 16\n".to_string();
    for instruction in instructions {
        disassembly += &format!("{}\n", instruction);
    }

    write(output_file, disassembly)
        .with_context(|| format!("could not write file {output_file:?}"))?;

    Ok(())
}

#[derive(Clone, Copy)]
pub struct SimulateLogOptions {
    log_ip: bool,
}

impl SimulateLogOptions {
    pub fn new(log_ip: bool) -> Self {
        Self { log_ip }
    }
}

pub struct LogContext {
    log: String,
    options: SimulateLogOptions,
}

impl LogContext {
    pub fn new(options: SimulateLogOptions) -> Self {
        Self {
            log: String::new(),
            options,
        }
    }

    pub fn log(&mut self, message: &str) {
        self.log += message;
    }
}

pub fn simulate(
    input_file: &str,
    log_file: &str,
    log_options: SimulateLogOptions,
) -> Result<(), anyhow::Error> {
    let machine_code =
        read(input_file).with_context(|| format!("could not read file {input_file:?}"))?;

    let mut i8086 = I8086::new(machine_code);

    let mut log_context = LogContext::new(log_options);

    //TODO unwrap
    log_context.log(&format!(
        "--- test\\{} execution ---\n",
        Path::new(input_file).file_name().unwrap().to_str().unwrap()
    ));

    while matches!(
        i8086.execute_next_instruction(&mut log_context)?,
        Status::Running
    ) {}

    log_context.log("\nFinal registers:\n");
    for id in 0..12 {
        let register = Register {
            id,
            data_type: DataType::Word,
        };
        let value = i8086.data_registers.get(register);
        if value != 0 {
            log_context.log(&format!(
                "      {}: 0x{:04x} ({})\n",
                register, value as u16, value as u16
            ));
        }
    }

    if log_context.options.log_ip {
        log_context.log(&format!(
            "      ip: 0x{:04x} ({})\n",
            i8086.instruction_pointer, i8086.instruction_pointer
        ));
    }

    if i8086.flags != Flags::default() {
        log_context.log(&format!("   flags: {}\n", i8086.flags));
    }

    log_context.log("\n");

    write(log_file, log_context.log)
        .with_context(|| format!("could not write file {log_file:?}"))?;

    Ok(())
}

struct I8086 {
    data_registers: Registers,
    flags: Flags,
    memory: Memory,
    instruction_pointer: u16,
}

fn do_op_impl(destination: i32, source: i32, operation: BinaryOperation) -> i32 {
    match operation {
        BinaryOperation::Mov => source,
        BinaryOperation::Add => destination + source,
        BinaryOperation::Sub | BinaryOperation::Cmp => destination - source,
    }
}

fn do_op(destination: i32, source: i32, operation: BinaryOperation) -> (i32, bool, bool) {
    let res = match operation {
        BinaryOperation::Mov => source,
        BinaryOperation::Add => destination + source,
        BinaryOperation::Sub | BinaryOperation::Cmp => destination - source,
    };

    let nibble_op = do_op_impl(
        (destination as u32 % 0x10) as i32,
        (source as u32 % 0x10) as i32,
        operation,
    );

    let word_op = do_op_impl(
        (destination as u32 % 0x10000) as i32,
        (source as u32 % 0x10000) as i32,
        operation,
    );

    let auxiliary_carry = nibble_op >> 4 != 0;
    let carry = word_op >> 16 != 0;

    (res, auxiliary_carry, carry)
}

enum Status {
    Halted,
    Running,
}

impl I8086 {
    fn new(machine_code: Vec<u8>) -> Self {
        let mut result = Self {
            data_registers: Registers { data: [0; 24] },
            flags: Flags::default(),
            memory: Memory { data: [0; 65536] },
            instruction_pointer: 0,
        };

        //TODO
        result.memory.data[..machine_code.len()].copy_from_slice(&machine_code);
        result.memory.data[machine_code.len()] = 0b11110100;
        result
    }

    fn read_next_instruction(&mut self) -> Result<Instruction, MachineCodeError> {
        let mut reader =
            MachineCodeReader::new(&self.memory.data[self.instruction_pointer as usize..]);
        let instruction = reader.next()?;
        self.instruction_pointer += reader.instruction_pointer as u16;
        Ok(instruction)
    }

    fn execute_next_instruction(
        &mut self,
        log_context: &mut LogContext,
    ) -> Result<Status, anyhow::Error> {
        let old_ip = self.instruction_pointer;
        let instruction = self.read_next_instruction()?;
        let instruction_size = self.instruction_pointer - old_ip;

        if matches!(instruction, Instruction::Halt) {
            self.instruction_pointer -= instruction_size;
        } else {
            log_context.log(&format!("{} ; ", instruction));
        }

        self.execute_instruction(instruction, log_context, instruction_size)
    }

    fn execute_binary_instruction(
        &mut self,
        binary_instruction: BinaryInstruction,
    ) -> Result<(), anyhow::Error> {
        //TODO: property of a Place?
        let data_type = binary_instruction.data_type();

        let old = self.get_value(Value::Place(binary_instruction.destination()), data_type);

        let source = self.get_value(binary_instruction.source(), data_type);

        let (signed_new, auxilary_carry, carry_flag) =
            do_op(old, source, binary_instruction.operation());

        if !matches!(binary_instruction.operation(), BinaryOperation::Cmp) {
            self.set_place(binary_instruction.destination(), data_type, signed_new);
        };

        if !matches!(binary_instruction.operation(), BinaryOperation::Mov) {
            let tamed_signed_new = match data_type {
                DataType::Byte => signed_new as i8 as i32,
                DataType::Word => signed_new as i16 as i32,
            };

            let unsigned_new = signed_new as u32;
            let tamed_unsigned_new = match data_type {
                DataType::Byte => unsigned_new as u8 as u32,
                DataType::Word => unsigned_new as u16 as u32,
            };

            self.flags.carry = carry_flag;
            self.flags.zero = tamed_unsigned_new == 0;
            self.flags.sign = tamed_signed_new < 0;
            self.flags.overflow = tamed_signed_new != signed_new;
            self.flags.auxuliary_carry = auxilary_carry;
            self.flags.parity = (tamed_unsigned_new % 0x100).count_ones() % 2 == 0;
        }

        Ok(())
    }

    fn execute_jump_instruction(&mut self, jump_instruction: JumpInstruction) {
        let do_jump = match jump_instruction.operation() {
            JumpOperation::Je => self.flags.zero,
            JumpOperation::Jl => !self.flags.sign && !self.flags.zero,
            JumpOperation::Jle => todo!("Jle"),
            JumpOperation::Jb => self.flags.sign,
            JumpOperation::Jbe => todo!("Jbe"),
            JumpOperation::Jp => self.flags.parity,
            JumpOperation::Jo => self.flags.overflow,
            JumpOperation::Js => self.flags.sign,
            JumpOperation::Jne => !self.flags.zero,
            JumpOperation::Jnl => todo!("Jnl"),
            JumpOperation::Jnle => todo!("Jnle"),
            JumpOperation::Jnb => todo!("Jnb"),
            JumpOperation::Jnbe => todo!("Jnbe"),
            JumpOperation::Jnp => todo!("Jnp"),
            JumpOperation::Jno => todo!("Jno"),
            JumpOperation::Jns => todo!("Jns"),
            JumpOperation::Loop => todo!("Loop"),
            JumpOperation::Loopz => todo!("Loopz"),
            JumpOperation::Loopnz => self.data_registers.do_loop() != 0,
            JumpOperation::Jcxz => todo!("Jcxz"),
        };

        if do_jump {
            self.instruction_pointer = self
                .instruction_pointer
                .wrapping_add(jump_instruction.offset() as u16);
        }
    }

    fn execute_instruction(
        &mut self,
        instruction: Instruction,
        log_context: &mut LogContext,
        instruction_size: u16,
    ) -> Result<Status, anyhow::Error> {
        let registers_before = self.data_registers.clone();
        let flags_before = self.flags;
        let instruction_pointer_before = self.instruction_pointer - instruction_size;

        match instruction {
            Instruction::Binary(binary_instruction) => {
                self.execute_binary_instruction(binary_instruction)?;
            }
            Instruction::Jump(jump_instruction) => {
                self.execute_jump_instruction(jump_instruction);
            }
            Instruction::Halt => return Ok(Status::Halted),
        };

        for register_id in 0..12 {
            let register = Register {
                id: register_id,
                data_type: DataType::Word,
            };
            let old = registers_before.get(register);
            let new = self.data_registers.get(register);
            if old != new {
                log_context.log(&format!(
                    "{}:{:#x}->{:#x} ",
                    register, old as u16, new as u16
                ));
            }
        }

        if log_context.options.log_ip {
            log_context.log(&format!(
                "ip:{:#x}->{:#x} ",
                instruction_pointer_before, self.instruction_pointer
            ));
        }

        if flags_before != self.flags {
            log_context.log(&format!("flags:{}->{} ", flags_before, self.flags));
        }

        log_context.log("\n");

        Ok(Status::Running)
    }

    fn get_address(&self, memory_place: MemoryPlace) -> i16 {
        memory_place.registers.map_or(0, |(reg_a, maybe_reg_b)| {
            self.data_registers.get(reg_a)
                + maybe_reg_b.map_or(0, |reg_b| self.data_registers.get(reg_b))
        }) as i16
            + memory_place.displacement
    }

    fn get_value(&self, source: Value, data_type: DataType) -> i32 {
        match source {
            Value::Place(Place::Memory(memory_place)) => {
                self.memory.get(self.get_address(memory_place), data_type)
            }
            Value::Immediate(literal) => literal as i32,
            Value::Place(Place::Register(register)) => self.data_registers.get(register),
        }
    }

    fn set_place(&mut self, destination: Place, data_type: DataType, res: i32) {
        match destination {
            Place::Register(register) => self.data_registers.set(register, res),
            Place::Memory(memory_place) => {
                self.memory
                    .set(self.get_address(memory_place), res, data_type)
            }
        }
    }
}

struct Memory {
    data: [u8; 65536],
}
impl Memory {
    fn get(&self, address: i16, data_type: DataType) -> i32 {
        let high = self.data[address as usize] as u32;
        match data_type {
            DataType::Byte => high as i32,
            DataType::Word => {
                let low = self.data[(address + 1) as usize] as u32;
                (low | (high << 8)) as i32
            }
        }
    }

    fn set(&mut self, address: i16, res: i32, data_type: DataType) {
        let low = res as u8;
        match data_type {
            DataType::Byte => {
                self.data[address as usize] = low;
            }
            DataType::Word => {
                let high = (res >> 8) as u8;
                self.data[address as usize] = high;
                self.data[(address + 1) as usize] = low;
            }
        }
    }
}

#[derive(Clone)]
struct Registers {
    data: [u8; 24],
}
impl Registers {
    fn do_loop(&mut self) -> i32 {
        let count = self.get(Register::CX) - 1;
        self.set(Register::CX, count);
        count
    }

    fn get(&self, register: Register) -> i32 {
        match register.data_type {
            DataType::Byte => self.data[register.id] as i32,
            DataType::Word => {
                let id = register.id * 2;
                let high = self.data[id] as u16;
                let low = self.data[id + 1] as u16;
                (low | (high << 8)) as i16 as i32
            }
        }
    }

    fn set(&mut self, register: Register, res: i32) {
        match register.data_type {
            DataType::Byte => self.data[register.id] = res as u8,
            DataType::Word => {
                let id = register.id * 2;
                let low = res as u8;
                let high = (res >> 8) as u8;
                self.data[id] = high;
                self.data[id + 1] = low;
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
struct Flags {
    auxuliary_carry: bool,
    carry: bool,
    overflow: bool,
    sign: bool,
    parity: bool,
    zero: bool,
}

impl Display for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.carry {
            write!(f, "C")?;
        }

        if self.parity {
            write!(f, "P")?;
        }

        if self.auxuliary_carry {
            write!(f, "A")?;
        }

        if self.sign {
            write!(f, "S")?;
        }

        if self.overflow {
            write!(f, "O")?;
        }

        if self.zero {
            write!(f, "Z")?;
        }

        Ok(())
    }
}
