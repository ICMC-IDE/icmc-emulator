#![feature(bigint_helper_methods)]

const IREG_FR: u16 = 0b000000;
const IREG_SP: u16 = 0b000001;
const IREG_PC: u16 = 0b000010;
const IREG_IR: u16 = 0b000011;
const IREG_KB: u16 = 0b000100;
const IREG_WC: u16 = 0b000101;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum State {
    Paused,
    BreakPoint,
    Halted,
    UnknownInstruction,
}

/// Reseting the Emulator is a common task, therefore it is convenient to have a copy of the original memory
pub struct Emulator {
    rom: Box<[u16]>,
    ram: Box<[u16]>,
    vram: Box<[u16]>,
    registers: [u16; 8],
    internal_registers: [u16; 64],
    state: State,
}

struct OpCode {}

impl OpCode {
    const ADD: u16 = 0b100000_0000000000;
    const AND: u16 = 0b010010_0000000000;
    const BKPT: u16 = 0b001110_0000000000;
    const CALL: u16 = 0b000011_0000000000;
    const CMP: u16 = 0b010110_0000000000;
    const DIV: u16 = 0b100011_0000000000;
    const HALT: u16 = 0b001111_0000000000;
    const INCDEC: u16 = 0b100100_0000000000;
    const JMP: u16 = 0b000010_0000000000;
    const LOAD: u16 = 0b110000_0000000000;
    const LOADI: u16 = 0b111100_0000000000;
    const LOADN: u16 = 0b111000_0000000000;
    const MOV: u16 = 0b110011_0000000000;
    const MUL: u16 = 0b100010_0000000000;
    const NOP: u16 = 0b000000_0000000000;
    const NOT: u16 = 0b010101_0000000000;
    const OR: u16 = 0b010011_0000000000;
    const POP: u16 = 0b000110_0000000000;
    const PUSH: u16 = 0b000101_0000000000;
    const READ: u16 = 0b110101_0000000000;
    const REM: u16 = 0b100101_0000000000;
    const RET: u16 = 0b000100_0000000000;
    const SETC: u16 = 0b001000_0000000000;
    const SHIFTROT: u16 = 0b010000_0000000000;
    const STORE: u16 = 0b110001_0000000000;
    const STOREI: u16 = 0b111101_0000000000;
    const SUB: u16 = 0b100001_0000000000;
    const WRITE: u16 = 0b110010_0000000000;
    const XOR: u16 = 0b010100_0000000000;
}

struct Flags {}

impl Flags {
    const ARITHMETIC_OVERFLOW: u16 = 0b0000000000100000;
    const CARRY: u16 = 0b0000000000010000;
    const COMP: u16 = Self::EQUAL | Self::LESS | Self::GREATER;
    const DIV_BY_ZERO: u16 = 0b0000000001000000;
    const EQUAL: u16 = 0b0000000000000100;
    const GREATER: u16 = 0b0000000000000001;
    const LESS: u16 = 0b0000000000000010;
    const NEGATIVE: u16 = 0b0000001000000000;
    const STACK_OVERFLOW: u16 = 0b0000000010000000;
    const STACK_UNDERFLOW: u16 = 0b0000000100000000;
    const ULA: u16 =
        Self::ARITHMETIC_OVERFLOW | Self::CARRY | Self::DIV_BY_ZERO | Self::NEGATIVE | Self::ZERO;
    const ZERO: u16 = 0b0000000000001000;
}

/// Public interface for the emulator.
impl Emulator {
    pub fn new() -> Self {
        let mut zelf = Self {
            rom: Box::new([0; 0x10000]),
            ram: Box::new([0; 0x10000]),
            vram: Box::new([0; 0x10000]),
            registers: [0; 8],
            internal_registers: [0; 64],
            state: State::Paused,
        };

        zelf.internal_registers[IREG_SP as usize] = u16::MAX;
        zelf.internal_registers[IREG_KB as usize] = 0xFF;
        zelf
    }

    pub fn load_program(&mut self, rom: &[u16]) {
        self.rom.copy_from_slice(rom);
        self.reset();
    }

    pub fn reset(&mut self) {
        self.ram.copy_from_slice(&self.rom);
        self.vram.fill(0);
        self.registers = [0, 0, 0, 0, 0, 0, 0, 0];
        self.internal_registers = [0; 64];
        self.internal_registers[IREG_SP as usize] = u16::MAX;
        self.internal_registers[IREG_KB as usize] = 0xFF;
        self.state = State::Paused;
    }

    pub const fn state(&self) -> State {
        self.state
    }

    pub const fn load(&self, address: u16) -> u16 {
        self.ram[address as usize]
    }

    pub const fn store(&mut self, address: u16, value: u16) {
        self.ram[address as usize] = value;
    }

    pub fn tick(&mut self, ticks: isize) -> isize {
        let mut handled_ticks = 0;

        while handled_ticks < ticks {
            if let Some(n) = self.next() {
                handled_ticks += n;
            } else {
                break;
            }
        }

        handled_ticks
    }

    pub const fn registers(&self) -> &[u16] {
        &self.registers
    }

    pub const fn internal_registers(&self) -> &[u16] {
        &self.internal_registers
    }

    pub const fn rom(&self) -> &[u16] {
        &self.rom
    }

    pub const fn ram(&self) -> &[u16] {
        &self.ram
    }

    pub const fn vram(&self) -> &[u16] {
        &self.vram
    }

    pub const fn reg_as_mut_ref(&mut self, reg: u16) -> &mut u16 {
        &mut self.registers[reg as usize]
    }

    pub const fn ireg_as_mut_ref(&mut self, reg: u16) -> &mut u16 {
        &mut self.internal_registers[reg as usize]
    }

    pub const fn reg(&self, reg: u16) -> u16 {
        self.registers[reg as usize]
    }

    pub const fn ireg(&self, reg: u16) -> u16 {
        self.internal_registers[reg as usize]
    }
}

/// Private interface for the emulator.
impl Emulator {
    const fn data(&mut self) -> u16 {
        let pc = self.ireg(IREG_PC);
        let ir = self.load(pc);
        *self.pc_as_mut_ref() = pc.wrapping_add(1);
        ir
    }

    const fn push(&mut self, value: u16) {
        self.store(self.sp(), value);

        let (sp, underflow) = self.sp().overflowing_sub(1);

        *self.sp_as_mut_ref() = sp;
        self.set_fr(Flags::STACK_UNDERFLOW, underflow);
    }

    const fn pop(&mut self) -> u16 {
        let (sp, overflow) = self.sp().overflowing_add(1);

        *self.sp_as_mut_ref() = sp;
        self.set_fr(Flags::STACK_OVERFLOW, overflow);
        self.load(self.sp())
    }

    const fn pc_as_mut_ref(&mut self) -> &mut u16 {
        self.ireg_as_mut_ref(IREG_PC)
    }

    const fn sp(&self) -> u16 {
        self.ireg(IREG_SP)
    }

    const fn sp_as_mut_ref(&mut self) -> &mut u16 {
        self.ireg_as_mut_ref(IREG_SP)
    }

    const fn fr(&self) -> u16 {
        self.ireg(IREG_FR)
    }

    const fn fr_as_mut_ref(&mut self) -> &mut u16 {
        self.ireg_as_mut_ref(IREG_FR)
    }

    const fn ir(&self) -> u16 {
        self.ireg(IREG_IR)
    }

    const fn fr_zero(&mut self) {
        let x = self.rx();
        let fr = self.fr();
        *self.fr_as_mut_ref() = (fr & !Flags::ULA) | (Flags::ZERO * (x == 0) as u16);
    }

    const fn set_fr(&mut self, mask: u16, value: bool) {
        let fr = *self.fr_as_mut_ref();
        *self.fr_as_mut_ref() = (fr & !mask) | (value as u16 * mask);
    }

    const fn rx_as_mut_ref(&mut self) -> &mut u16 {
        self.reg_as_mut_ref((self.ir() & 0b0000001110000000) >> 7)
    }

    const fn ri_as_mut_ref(&mut self) -> &mut u16 {
        self.ireg_as_mut_ref(self.ir() & 0b0000000000111111)
    }

    const fn rx(&self) -> u16 {
        self.reg((self.ir() & 0b0000001110000000) >> 7)
    }

    const fn ry(&self) -> u16 {
        self.reg((self.ir() & 0b0000000001110000) >> 4)
    }

    const fn rz(&self) -> u16 {
        self.reg((self.ir() & 0b0000000000001110) >> 1)
    }

    const fn ri(&self) -> u16 {
        self.ireg(self.ir() & 0b0000000000111111)
    }

    const fn opcode(&mut self) -> u16 {
        self.ir() & 0b1111110000000000
    }

    const fn c(&self) -> u16 {
        self.ir() & 0b0000000000000001
    }

    const fn test(&self, cond: u16) -> bool {
        let fr = self.fr();
        match cond {
            0b0000 => true,
            0b0001 if fr & Flags::EQUAL == Flags::EQUAL => true,
            0b0010 if fr & Flags::EQUAL == 0 => true,
            0b0011 if fr & Flags::ZERO == Flags::ZERO => true,
            0b0100 if fr & Flags::ZERO == 0 => true,
            0b0101 if fr & Flags::CARRY == Flags::CARRY => true,
            0b0110 if fr & Flags::CARRY == 0 => true,
            0b0111 if fr & Flags::GREATER == Flags::GREATER => true,
            0b1000 if fr & Flags::LESS == Flags::LESS => true,
            0b1001
                if (fr & Flags::EQUAL == Flags::EQUAL || fr & Flags::GREATER == Flags::GREATER) =>
            {
                true
            }
            0b1010 if (fr & Flags::EQUAL == Flags::EQUAL || fr & Flags::LESS == Flags::LESS) => {
                true
            }
            0b1011 if fr & Flags::ARITHMETIC_OVERFLOW == Flags::ARITHMETIC_OVERFLOW => true,
            0b1100 if fr & Flags::ARITHMETIC_OVERFLOW == 0 => true,
            0b1101 if fr & Flags::NEGATIVE == Flags::NEGATIVE => true,
            0b1110 if fr & Flags::DIV_BY_ZERO == Flags::DIV_BY_ZERO => true,
            _ => false,
        }
    }
}

impl Iterator for Emulator {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            State::Halted => return None,
            State::BreakPoint => self.state = State::Paused,
            _ => (),
        };

        *self.ireg_as_mut_ref(IREG_IR) = self.data();
        let opcode = self.opcode();

        match opcode {
            OpCode::STORE => {
                let address = self.data();
                let x = self.rx();
                self.store(address, x);
                Some(3)
            }
            OpCode::LOAD => {
                let address = self.data();
                *self.rx_as_mut_ref() = self.load(address);
                Some(3)
            }
            OpCode::STOREI => {
                let x = self.rx();
                self.store(x, self.ry());
                Some(2)
            }
            OpCode::LOADI => {
                *self.rx_as_mut_ref() = self.load(self.ry());
                Some(2)
            }
            OpCode::LOADN => {
                let number = self.data();
                *self.rx_as_mut_ref() = number;
                Some(2)
            }
            OpCode::MOV => {
                let mode = self.ir() & 0b11;
                let value = match mode {
                    0b01 => self.sp(),
                    0b11 => self.rx(),
                    _ => self.ry(),
                };

                *match mode {
                    0b011 => self.sp_as_mut_ref(),
                    _ => self.rx_as_mut_ref(),
                } = value;
                Some(2)
            }
            OpCode::ADD => {
                let carry = self.fr() & Flags::CARRY == Flags::CARRY;
                let (value, carry) = self.ry().carrying_add(self.rz(), self.c() == 0b1 && carry);

                *self.rx_as_mut_ref() = value;
                self.set_fr(Flags::CARRY, carry);
                self.fr_zero();
                Some(2)
            }
            OpCode::SUB => {
                let carry = self.fr() & Flags::CARRY == Flags::CARRY;
                let (value, carry1) = self.ry().carrying_add(!self.rz(), self.c() == 0b1 && carry);
                let (value, carry2) = value.overflowing_add(1);

                *self.rx_as_mut_ref() = value;
                self.set_fr(Flags::CARRY, carry1 | carry2);
                self.fr_zero();
                Some(2)
            }
            OpCode::MUL => {
                let carry = self.fr() & Flags::CARRY == Flags::CARRY;
                let (value, overflow) = self
                    .ry()
                    .carrying_mul(self.rz(), (self.c() == 0b1 && carry) as u16);

                *self.rx_as_mut_ref() = value;
                self.set_fr(Flags::ARITHMETIC_OVERFLOW, overflow != 0);
                self.fr_zero();
                Some(2)
            }
            OpCode::DIV => {
                let carry = self.fr() & Flags::CARRY == Flags::CARRY;
                let (value, overflow) = (if self.rz() == 0 {
                    self.ry()
                } else {
                    self.ry() / self.rz()
                })
                .overflowing_add((self.c() == 0b1 && carry) as u16);

                *self.rx_as_mut_ref() = value;
                self.set_fr(Flags::DIV_BY_ZERO, self.rz() == 0);
                self.set_fr(Flags::ARITHMETIC_OVERFLOW, overflow);
                self.fr_zero();
                Some(2)
            }
            OpCode::INCDEC => {
                let (value, overflow) = if self.ir() & 0b1000000 == 0b1000000 {
                    self.rx().overflowing_sub(0b1)
                } else {
                    self.rx().overflowing_add(0b1)
                };

                *self.rx_as_mut_ref() = value;
                self.set_fr(Flags::ARITHMETIC_OVERFLOW, overflow);
                self.fr_zero();
                Some(2)
            }
            OpCode::REM => {
                *self.rx_as_mut_ref() = if self.rz() == 0 {
                    self.ry()
                } else {
                    self.ry() % self.rz()
                };
                self.set_fr(Flags::DIV_BY_ZERO, self.rz() == 0);
                self.fr_zero();
                Some(2)
            }
            OpCode::AND => {
                *self.rx_as_mut_ref() = self.ry() & self.rz();
                self.fr_zero();
                Some(2)
            }
            OpCode::OR => {
                *self.rx_as_mut_ref() = self.ry() | self.rz();
                self.fr_zero();
                Some(2)
            }
            OpCode::XOR => {
                *self.rx_as_mut_ref() = self.ry() ^ self.rz();
                self.fr_zero();
                Some(2)
            }
            OpCode::NOT => {
                *self.rx_as_mut_ref() = !self.ry();
                self.fr_zero();
                Some(2)
            }
            OpCode::SHIFTROT => {
                let mode = (self.ir() & 0b1110000) >> 4;
                let n = self.ir() & 0b1111;

                let (value, overflow) = match mode {
                    0b000 => self.rx().overflowing_shl(n as u32),
                    0b001 => {
                        let mut res = self.rx().overflowing_shl(n as u32);
                        res.0 |= (1 << n) - 1;
                        res
                    }
                    0b010 => self.rx().overflowing_shr(n as u32),
                    0b011 => {
                        let mut res = self.rx().overflowing_shr(n as u32);
                        res.0 |= ((1 << n) - 1) << (16 - n);
                        res
                    }
                    0b100 | 0b101 => (self.rx().rotate_left(n as u32), false),
                    0b110 | 0b111 => (self.rx().rotate_right(n as u32), false),
                    _ => {
                        self.state = State::UnknownInstruction;
                        return None;
                    }
                };

                *self.rx_as_mut_ref() = value;
                self.set_fr(Flags::ARITHMETIC_OVERFLOW, overflow);
                self.fr_zero();
                Some(2)
            }
            OpCode::CMP => {
                let x = self.rx();

                *self.fr_as_mut_ref() = (self.fr() & !Flags::COMP)
                    | match x.cmp(&self.ry()) {
                        core::cmp::Ordering::Less => Flags::LESS,
                        core::cmp::Ordering::Equal => Flags::EQUAL,
                        core::cmp::Ordering::Greater => Flags::GREATER,
                    };
                self.fr_zero();
                Some(2)
            }
            OpCode::READ => {
                *self.rx_as_mut_ref() = self.ireg(IREG_KB);
                Some(2)
            }
            OpCode::WRITE => {
                let byte = self.rx();
                let offset = self.ry();

                self.vram[offset as usize] = byte;
                *self.ireg_as_mut_ref(IREG_WC) = self.ireg(IREG_WC).wrapping_add(1);

                Some(2)
            }
            OpCode::JMP => {
                let cond = (self.ir() & 0b1111000000) >> 6;
                let address = self.data();

                if self.test(cond) {
                    *self.pc_as_mut_ref() = address;
                }
                Some(3)
            }
            OpCode::CALL => {
                let cond = (self.ir() & 0b1111000000) >> 6;
                let address = self.data();

                if self.test(cond) {
                    let pc = *self.pc_as_mut_ref();
                    self.push(pc);
                    *self.pc_as_mut_ref() = address;
                }
                Some(3)
            }
            OpCode::RET => {
                *self.pc_as_mut_ref() = self.pop();
                Some(4)
            }
            OpCode::PUSH => {
                let value = if self.ir() & 0b1000000 == 0b1000000 {
                    self.ri()
                } else {
                    self.rx()
                };

                self.push(value);
                Some(2)
            }
            OpCode::POP => {
                let value = self.pop();

                *if self.ir() & 0b1000000 == 0b1000000 {
                    self.ri_as_mut_ref()
                } else {
                    self.rx_as_mut_ref()
                } = value;
                Some(3)
            }
            OpCode::SETC => {
                let c = self.fr() & 0b1000000000 == 0b1000000000;
                self.set_fr(Flags::CARRY, c);
                Some(2)
            }
            OpCode::HALT => {
                self.state = State::Halted;
                None
            }
            OpCode::BKPT => {
                self.state = State::BreakPoint;
                None
            }
            OpCode::NOP => Some(2),
            _ => {
                self.state = State::UnknownInstruction;
                None
            }
        }
    }
}
