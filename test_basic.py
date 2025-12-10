#!/usr/bin/env python3
"""
Simple 6502 emulator to test MS-BASIC binary.
Memory-mapped I/O:
  $FFF0 - CHAROUT (write)
  $FFF1 - CHARIN (read)
  $FFF2 - STATUS (bit 0 = char available)
"""

import sys
import select
import tty
import termios

# Memory
memory = bytearray(65536)

# Registers
A = 0    # Accumulator
X = 0    # X index
Y = 0    # Y index
SP = 0xFF  # Stack pointer
PC = 0   # Program counter
P = 0x24  # Status: --1--I-- (IRQ disabled, bit 5 always 1)

# Status flag bits
N_FLAG = 0x80  # Negative
V_FLAG = 0x40  # Overflow
B_FLAG = 0x10  # Break
D_FLAG = 0x08  # Decimal
I_FLAG = 0x04  # IRQ disable
Z_FLAG = 0x02  # Zero
C_FLAG = 0x01  # Carry

# I/O addresses
IO_CHAROUT = 0xFFF0
IO_CHARIN = 0xFFF1
IO_STATUS = 0xFFF2
IO_PEEK = 0xFFF3  # Peek at next char without consuming

input_buffer = []
output_buffer = []
running = True
cycle_count = 0

def set_nz(value):
    """Set N and Z flags based on value."""
    global P
    P = P & ~(N_FLAG | Z_FLAG)
    if value == 0:
        P |= Z_FLAG
    if value & 0x80:
        P |= N_FLAG
    return value & 0xFF

def push(value):
    """Push byte onto stack."""
    global SP
    memory[0x100 + SP] = value & 0xFF
    SP = (SP - 1) & 0xFF

def pull():
    """Pull byte from stack."""
    global SP
    SP = (SP + 1) & 0xFF
    return memory[0x100 + SP]

def push_word(value):
    """Push 16-bit value onto stack (high byte first)."""
    push((value >> 8) & 0xFF)
    push(value & 0xFF)

def pull_word():
    """Pull 16-bit value from stack."""
    lo = pull()
    hi = pull()
    return (hi << 8) | lo

io_debug = False
batch_mode = False

def read_byte(addr):
    """Read byte from memory with I/O handling."""
    global io_debug
    addr &= 0xFFFF
    if addr == IO_CHARIN:
        if input_buffer:
            ch = input_buffer.pop(0)
            if io_debug:
                print(f"[CHARIN: {chr(ch) if 32 <= ch < 127 else f'${ch:02X}'}]", end='', flush=True)
            # Echo input character to output (BASIC doesn't echo automatically)
            write_byte(IO_CHAROUT, ch)
            return ch
        return 0
    elif addr == IO_STATUS:
        status = 1 if input_buffer else 0
        return status
    elif addr == IO_PEEK:
        # Peek at next char without consuming (for Ctrl-C check)
        if input_buffer:
            return input_buffer[0]
        return 0
    return memory[addr]

def write_byte(addr, value):
    """Write byte to memory with I/O handling."""
    global running, batch_mode
    addr &= 0xFFFF
    value &= 0xFF
    if addr == IO_CHAROUT:
        if not batch_mode:
            ch = chr(value) if 32 <= value < 127 or value in (10, 13) else f'[{value:02X}]'
            sys.stdout.write(ch)
            sys.stdout.flush()
        output_buffer.append(value)
    elif addr >= 0x0800 and addr < 0xFFF0:
        # ROM area ($0800+) - writes are ignored after binary is loaded
        # This makes memory detection stop at the ROM boundary
        pass
    else:
        memory[addr] = value

def read_word(addr):
    """Read 16-bit word (little-endian)."""
    return read_byte(addr) | (read_byte(addr + 1) << 8)

def fetch():
    """Fetch byte at PC and increment PC."""
    global PC
    value = read_byte(PC)
    PC = (PC + 1) & 0xFFFF
    return value

def fetch_word():
    """Fetch 16-bit word at PC."""
    lo = fetch()
    hi = fetch()
    return (hi << 8) | lo

# Addressing modes
def addr_imm():
    return fetch()

def addr_zp():
    return fetch()

def addr_zpx():
    return (fetch() + X) & 0xFF

def addr_zpy():
    return (fetch() + Y) & 0xFF

def addr_abs():
    return fetch_word()

def addr_absx():
    return (fetch_word() + X) & 0xFFFF

def addr_absy():
    return (fetch_word() + Y) & 0xFFFF

def addr_indx():
    zp = (fetch() + X) & 0xFF
    return read_byte(zp) | (read_byte((zp + 1) & 0xFF) << 8)

def addr_indy():
    zp = fetch()
    base = read_byte(zp) | (read_byte((zp + 1) & 0xFF) << 8)
    return (base + Y) & 0xFFFF

def branch(condition):
    """Handle branch instruction."""
    global PC
    offset = fetch()
    if offset & 0x80:
        offset -= 256
    if condition:
        PC = (PC + offset) & 0xFFFF

def execute():
    """Execute one instruction."""
    global A, X, Y, SP, PC, P, running, cycle_count

    opcode = fetch()
    cycle_count += 1

    # LDA
    if opcode == 0xA9: A = set_nz(fetch())  # LDA #imm
    elif opcode == 0xA5: A = set_nz(read_byte(addr_zp()))  # LDA zp
    elif opcode == 0xB5: A = set_nz(read_byte(addr_zpx()))  # LDA zp,X
    elif opcode == 0xAD: A = set_nz(read_byte(addr_abs()))  # LDA abs
    elif opcode == 0xBD: A = set_nz(read_byte(addr_absx()))  # LDA abs,X
    elif opcode == 0xB9: A = set_nz(read_byte(addr_absy()))  # LDA abs,Y
    elif opcode == 0xA1: A = set_nz(read_byte(addr_indx()))  # LDA (zp,X)
    elif opcode == 0xB1: A = set_nz(read_byte(addr_indy()))  # LDA (zp),Y

    # LDX
    elif opcode == 0xA2: X = set_nz(fetch())  # LDX #imm
    elif opcode == 0xA6: X = set_nz(read_byte(addr_zp()))  # LDX zp
    elif opcode == 0xB6: X = set_nz(read_byte(addr_zpy()))  # LDX zp,Y
    elif opcode == 0xAE: X = set_nz(read_byte(addr_abs()))  # LDX abs
    elif opcode == 0xBE: X = set_nz(read_byte(addr_absy()))  # LDX abs,Y

    # LDY
    elif opcode == 0xA0: Y = set_nz(fetch())  # LDY #imm
    elif opcode == 0xA4: Y = set_nz(read_byte(addr_zp()))  # LDY zp
    elif opcode == 0xB4: Y = set_nz(read_byte(addr_zpx()))  # LDY zp,X
    elif opcode == 0xAC: Y = set_nz(read_byte(addr_abs()))  # LDY abs
    elif opcode == 0xBC: Y = set_nz(read_byte(addr_absx()))  # LDY abs,X

    # STA
    elif opcode == 0x85: write_byte(addr_zp(), A)  # STA zp
    elif opcode == 0x95: write_byte(addr_zpx(), A)  # STA zp,X
    elif opcode == 0x8D: write_byte(addr_abs(), A)  # STA abs
    elif opcode == 0x9D: write_byte(addr_absx(), A)  # STA abs,X
    elif opcode == 0x99: write_byte(addr_absy(), A)  # STA abs,Y
    elif opcode == 0x81: write_byte(addr_indx(), A)  # STA (zp,X)
    elif opcode == 0x91: write_byte(addr_indy(), A)  # STA (zp),Y

    # STX
    elif opcode == 0x86: write_byte(addr_zp(), X)  # STX zp
    elif opcode == 0x96: write_byte(addr_zpy(), X)  # STX zp,Y
    elif opcode == 0x8E: write_byte(addr_abs(), X)  # STX abs

    # STY
    elif opcode == 0x84: write_byte(addr_zp(), Y)  # STY zp
    elif opcode == 0x94: write_byte(addr_zpx(), Y)  # STY zp,X
    elif opcode == 0x8C: write_byte(addr_abs(), Y)  # STY abs

    # Transfers
    elif opcode == 0xAA: X = set_nz(A)  # TAX
    elif opcode == 0x8A: A = set_nz(X)  # TXA
    elif opcode == 0xA8: Y = set_nz(A)  # TAY
    elif opcode == 0x98: A = set_nz(Y)  # TYA
    elif opcode == 0xBA: X = set_nz(SP)  # TSX
    elif opcode == 0x9A: SP = X  # TXS

    # Stack
    elif opcode == 0x48: push(A)  # PHA
    elif opcode == 0x68: A = set_nz(pull())  # PLA
    elif opcode == 0x08: push(P | 0x30)  # PHP
    elif opcode == 0x28: P = (pull() & 0xEF) | 0x20  # PLP

    # ADC
    elif opcode in (0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71):
        if opcode == 0x69: val = fetch()
        elif opcode == 0x65: val = read_byte(addr_zp())
        elif opcode == 0x75: val = read_byte(addr_zpx())
        elif opcode == 0x6D: val = read_byte(addr_abs())
        elif opcode == 0x7D: val = read_byte(addr_absx())
        elif opcode == 0x79: val = read_byte(addr_absy())
        elif opcode == 0x61: val = read_byte(addr_indx())
        elif opcode == 0x71: val = read_byte(addr_indy())

        carry = 1 if (P & C_FLAG) else 0
        result = A + val + carry
        P = P & ~(C_FLAG | V_FLAG)
        if result > 255:
            P |= C_FLAG
        if ((A ^ result) & (val ^ result) & 0x80):
            P |= V_FLAG
        A = set_nz(result & 0xFF)

    # SBC
    elif opcode in (0xE9, 0xE5, 0xF5, 0xED, 0xFD, 0xF9, 0xE1, 0xF1):
        if opcode == 0xE9: val = fetch()
        elif opcode == 0xE5: val = read_byte(addr_zp())
        elif opcode == 0xF5: val = read_byte(addr_zpx())
        elif opcode == 0xED: val = read_byte(addr_abs())
        elif opcode == 0xFD: val = read_byte(addr_absx())
        elif opcode == 0xF9: val = read_byte(addr_absy())
        elif opcode == 0xE1: val = read_byte(addr_indx())
        elif opcode == 0xF1: val = read_byte(addr_indy())

        carry = 1 if (P & C_FLAG) else 0
        result = A - val - (1 - carry)
        P = P & ~(C_FLAG | V_FLAG)
        if result >= 0:
            P |= C_FLAG
        if ((A ^ val) & (A ^ result) & 0x80):
            P |= V_FLAG
        A = set_nz(result & 0xFF)

    # AND
    elif opcode == 0x29: A = set_nz(A & fetch())
    elif opcode == 0x25: A = set_nz(A & read_byte(addr_zp()))
    elif opcode == 0x35: A = set_nz(A & read_byte(addr_zpx()))
    elif opcode == 0x2D: A = set_nz(A & read_byte(addr_abs()))
    elif opcode == 0x3D: A = set_nz(A & read_byte(addr_absx()))
    elif opcode == 0x39: A = set_nz(A & read_byte(addr_absy()))
    elif opcode == 0x21: A = set_nz(A & read_byte(addr_indx()))
    elif opcode == 0x31: A = set_nz(A & read_byte(addr_indy()))

    # ORA
    elif opcode == 0x09: A = set_nz(A | fetch())
    elif opcode == 0x05: A = set_nz(A | read_byte(addr_zp()))
    elif opcode == 0x15: A = set_nz(A | read_byte(addr_zpx()))
    elif opcode == 0x0D: A = set_nz(A | read_byte(addr_abs()))
    elif opcode == 0x1D: A = set_nz(A | read_byte(addr_absx()))
    elif opcode == 0x19: A = set_nz(A | read_byte(addr_absy()))
    elif opcode == 0x01: A = set_nz(A | read_byte(addr_indx()))
    elif opcode == 0x11: A = set_nz(A | read_byte(addr_indy()))

    # EOR
    elif opcode == 0x49: A = set_nz(A ^ fetch())
    elif opcode == 0x45: A = set_nz(A ^ read_byte(addr_zp()))
    elif opcode == 0x55: A = set_nz(A ^ read_byte(addr_zpx()))
    elif opcode == 0x4D: A = set_nz(A ^ read_byte(addr_abs()))
    elif opcode == 0x5D: A = set_nz(A ^ read_byte(addr_absx()))
    elif opcode == 0x59: A = set_nz(A ^ read_byte(addr_absy()))
    elif opcode == 0x41: A = set_nz(A ^ read_byte(addr_indx()))
    elif opcode == 0x51: A = set_nz(A ^ read_byte(addr_indy()))

    # CMP
    elif opcode in (0xC9, 0xC5, 0xD5, 0xCD, 0xDD, 0xD9, 0xC1, 0xD1):
        if opcode == 0xC9: val = fetch()
        elif opcode == 0xC5: val = read_byte(addr_zp())
        elif opcode == 0xD5: val = read_byte(addr_zpx())
        elif opcode == 0xCD: val = read_byte(addr_abs())
        elif opcode == 0xDD: val = read_byte(addr_absx())
        elif opcode == 0xD9: val = read_byte(addr_absy())
        elif opcode == 0xC1: val = read_byte(addr_indx())
        elif opcode == 0xD1: val = read_byte(addr_indy())

        result = A - val
        P = P & ~C_FLAG
        if A >= val:
            P |= C_FLAG
        set_nz(result & 0xFF)

    # CPX
    elif opcode == 0xE0:  # CPX #imm
        val = fetch()
        P = P & ~C_FLAG
        if X >= val: P |= C_FLAG
        set_nz((X - val) & 0xFF)
    elif opcode == 0xE4:  # CPX zp
        val = read_byte(addr_zp())
        P = P & ~C_FLAG
        if X >= val: P |= C_FLAG
        set_nz((X - val) & 0xFF)
    elif opcode == 0xEC:  # CPX abs
        val = read_byte(addr_abs())
        P = P & ~C_FLAG
        if X >= val: P |= C_FLAG
        set_nz((X - val) & 0xFF)

    # CPY
    elif opcode == 0xC0:  # CPY #imm
        val = fetch()
        P = P & ~C_FLAG
        if Y >= val: P |= C_FLAG
        set_nz((Y - val) & 0xFF)
    elif opcode == 0xC4:  # CPY zp
        val = read_byte(addr_zp())
        P = P & ~C_FLAG
        if Y >= val: P |= C_FLAG
        set_nz((Y - val) & 0xFF)
    elif opcode == 0xCC:  # CPY abs
        val = read_byte(addr_abs())
        P = P & ~C_FLAG
        if Y >= val: P |= C_FLAG
        set_nz((Y - val) & 0xFF)

    # INC
    elif opcode == 0xE6:  # INC zp
        addr = addr_zp()
        write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))
    elif opcode == 0xF6:  # INC zp,X
        addr = addr_zpx()
        write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))
    elif opcode == 0xEE:  # INC abs
        addr = addr_abs()
        write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))
    elif opcode == 0xFE:  # INC abs,X
        addr = addr_absx()
        write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))

    # DEC
    elif opcode == 0xC6:  # DEC zp
        addr = addr_zp()
        write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))
    elif opcode == 0xD6:  # DEC zp,X
        addr = addr_zpx()
        write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))
    elif opcode == 0xCE:  # DEC abs
        addr = addr_abs()
        write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))
    elif opcode == 0xDE:  # DEC abs,X
        addr = addr_absx()
        write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))

    elif opcode == 0xE8: X = set_nz((X + 1) & 0xFF)  # INX
    elif opcode == 0xCA: X = set_nz((X - 1) & 0xFF)  # DEX
    elif opcode == 0xC8: Y = set_nz((Y + 1) & 0xFF)  # INY
    elif opcode == 0x88: Y = set_nz((Y - 1) & 0xFF)  # DEY

    # ASL
    elif opcode == 0x0A:  # ASL A
        P = P & ~C_FLAG
        if A & 0x80: P |= C_FLAG
        A = set_nz((A << 1) & 0xFF)
    elif opcode == 0x06:  # ASL zp
        addr = addr_zp()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz((val << 1) & 0xFF))
    elif opcode == 0x16:  # ASL zp,X
        addr = addr_zpx()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz((val << 1) & 0xFF))
    elif opcode == 0x0E:  # ASL abs
        addr = addr_abs()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz((val << 1) & 0xFF))
    elif opcode == 0x1E:  # ASL abs,X
        addr = addr_absx()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz((val << 1) & 0xFF))

    # LSR
    elif opcode == 0x4A:  # LSR A
        P = P & ~C_FLAG
        if A & 0x01: P |= C_FLAG
        A = set_nz(A >> 1)
    elif opcode == 0x46:  # LSR zp
        addr = addr_zp()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz(val >> 1))
    elif opcode == 0x56:  # LSR zp,X
        addr = addr_zpx()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz(val >> 1))
    elif opcode == 0x4E:  # LSR abs
        addr = addr_abs()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz(val >> 1))
    elif opcode == 0x5E:  # LSR abs,X
        addr = addr_absx()
        val = read_byte(addr)
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz(val >> 1))

    # ROL
    elif opcode == 0x2A:  # ROL A
        carry = 1 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if A & 0x80: P |= C_FLAG
        A = set_nz(((A << 1) | carry) & 0xFF)
    elif opcode == 0x26:  # ROL zp
        addr = addr_zp()
        val = read_byte(addr)
        carry = 1 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))
    elif opcode == 0x36:  # ROL zp,X
        addr = addr_zpx()
        val = read_byte(addr)
        carry = 1 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))
    elif opcode == 0x2E:  # ROL abs
        addr = addr_abs()
        val = read_byte(addr)
        carry = 1 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))
    elif opcode == 0x3E:  # ROL abs,X
        addr = addr_absx()
        val = read_byte(addr)
        carry = 1 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x80: P |= C_FLAG
        write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))

    # ROR
    elif opcode == 0x6A:  # ROR A
        carry = 0x80 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if A & 0x01: P |= C_FLAG
        A = set_nz((A >> 1) | carry)
    elif opcode == 0x66:  # ROR zp
        addr = addr_zp()
        val = read_byte(addr)
        carry = 0x80 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz((val >> 1) | carry))
    elif opcode == 0x76:  # ROR zp,X
        addr = addr_zpx()
        val = read_byte(addr)
        carry = 0x80 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz((val >> 1) | carry))
    elif opcode == 0x6E:  # ROR abs
        addr = addr_abs()
        val = read_byte(addr)
        carry = 0x80 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz((val >> 1) | carry))
    elif opcode == 0x7E:  # ROR abs,X
        addr = addr_absx()
        val = read_byte(addr)
        carry = 0x80 if (P & C_FLAG) else 0
        P = P & ~C_FLAG
        if val & 0x01: P |= C_FLAG
        write_byte(addr, set_nz((val >> 1) | carry))

    # BIT
    elif opcode == 0x24:  # BIT zp
        val = read_byte(addr_zp())
        P = P & ~(N_FLAG | V_FLAG | Z_FLAG)
        if val & 0x80: P |= N_FLAG
        if val & 0x40: P |= V_FLAG
        if (A & val) == 0: P |= Z_FLAG
    elif opcode == 0x2C:  # BIT abs
        val = read_byte(addr_abs())
        P = P & ~(N_FLAG | V_FLAG | Z_FLAG)
        if val & 0x80: P |= N_FLAG
        if val & 0x40: P |= V_FLAG
        if (A & val) == 0: P |= Z_FLAG

    # Branches
    elif opcode == 0x10: branch(not (P & N_FLAG))  # BPL
    elif opcode == 0x30: branch(P & N_FLAG)  # BMI
    elif opcode == 0x50: branch(not (P & V_FLAG))  # BVC
    elif opcode == 0x70: branch(P & V_FLAG)  # BVS
    elif opcode == 0x90: branch(not (P & C_FLAG))  # BCC
    elif opcode == 0xB0: branch(P & C_FLAG)  # BCS
    elif opcode == 0xD0: branch(not (P & Z_FLAG))  # BNE
    elif opcode == 0xF0: branch(P & Z_FLAG)  # BEQ

    # JMP
    elif opcode == 0x4C:  # JMP abs
        PC = fetch_word()
    elif opcode == 0x6C:  # JMP (ind)
        addr = fetch_word()
        # 6502 bug: wrap within page
        lo = read_byte(addr)
        hi = read_byte((addr & 0xFF00) | ((addr + 1) & 0xFF))
        PC = (hi << 8) | lo

    # JSR/RTS
    elif opcode == 0x20:  # JSR
        addr = fetch_word()
        push_word(PC - 1)
        PC = addr
    elif opcode == 0x60:  # RTS
        PC = (pull_word() + 1) & 0xFFFF

    # RTI
    elif opcode == 0x40:
        P = (pull() & 0xEF) | 0x20
        PC = pull_word()

    # BRK
    elif opcode == 0x00:
        PC = (PC + 1) & 0xFFFF
        push_word(PC)
        push(P | B_FLAG)
        P |= I_FLAG
        PC = read_word(0xFFFE)

    # Flags
    elif opcode == 0x18: P &= ~C_FLAG  # CLC
    elif opcode == 0x38: P |= C_FLAG   # SEC
    elif opcode == 0x58: P &= ~I_FLAG  # CLI
    elif opcode == 0x78: P |= I_FLAG   # SEI
    elif opcode == 0xB8: P &= ~V_FLAG  # CLV
    elif opcode == 0xD8: P &= ~D_FLAG  # CLD
    elif opcode == 0xF8: P |= D_FLAG   # SED

    # NOP
    elif opcode == 0xEA: pass

    else:
        print(f"\nUnknown opcode: ${opcode:02X} at ${PC-1:04X}")
        print(f"Registers: A=${A:02X} X=${X:02X} Y=${Y:02X} SP=${SP:02X}")
        running = False

def load_binary(filename, address):
    """Load binary file into memory."""
    with open(filename, 'rb') as f:
        data = f.read()
    for i, byte in enumerate(data):
        memory[address + i] = byte
    return len(data)

def check_stdin():
    """Check if there's input available on stdin (non-blocking)."""
    return select.select([sys.stdin], [], [], 0)[0]

def run_interactive():
    """Run BASIC interactively with terminal I/O."""
    global running, input_buffer

    # Save terminal settings
    old_settings = termios.tcgetattr(sys.stdin)

    try:
        # Set terminal to raw mode
        tty.setraw(sys.stdin.fileno())

        print("\r\nMS-BASIC for ABAP 6502 Emulator\r")
        print("Press Ctrl-C to exit\r\n")

        while running:
            # Check for input
            if check_stdin():
                ch = sys.stdin.read(1)
                if ch == '\x03':  # Ctrl-C
                    print("\r\n^C - Exiting\r")
                    break
                # Convert CR/LF to CR for BASIC
                if ch == '\r' or ch == '\n':
                    input_buffer.append(13)
                else:
                    # Lowercase is converted to uppercase in read_byte()
                    input_buffer.append(ord(ch))

            # Execute instructions
            for _ in range(1000):  # Batch execution
                if not running:
                    break
                execute()

    except KeyboardInterrupt:
        print("\r\nInterrupted\r")
    finally:
        # Restore terminal settings
        termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)

def run_batch(commands, max_cycles=10000000):
    """Run BASIC with pre-defined input commands."""
    global running, input_buffer, cycle_count

    # Queue all input
    for cmd in commands:
        for ch in cmd:
            input_buffer.append(ord(ch))
        input_buffer.append(13)  # CR after each command

    global io_debug, batch_mode
    io_debug = False  # Disable I/O debug output
    batch_mode = True  # Suppress real-time output
    print(f"Input buffer has {len(input_buffer)} characters")

    # Run until output stops changing or max cycles
    last_output_len = 0
    stable_count = 0
    output_check_interval = 10000

    while running and cycle_count < max_cycles:
        execute()

        # Periodic progress check
        if cycle_count % output_check_interval == 0:
            # Check if we're waiting for input with no more commands
            if not input_buffer and len(output_buffer) == last_output_len:
                stable_count += 1
                if stable_count > 10:  # Waiting for input
                    break
            else:
                stable_count = 0
                last_output_len = len(output_buffer)

    return ''.join(chr(b) if 32 <= b < 127 or b in (10, 13) else '' for b in output_buffer)

def main():
    global PC

    # Load BASIC
    binary_path = "bin/msbasic.bin"
    try:
        size = load_binary(binary_path, 0x0800)
        print(f"Loaded {size} bytes at $0800")
    except FileNotFoundError:
        print(f"Error: {binary_path} not found. Run ./make.sh in msbasic/ first.")
        return 1

    # Set entry point
    PC = 0x272D  # COLD_START
    print(f"Starting at ${PC:04X}")

    # Check for batch mode
    if len(sys.argv) > 1 and sys.argv[1] == '--batch':
        # Batch test
        commands = [
            '',  # Accept default memory size
            '',  # Accept default terminal width
            'PRINT "HELLO WORLD"',
            'PRINT 2+2',
            '10 PRINT "TEST"',
            '20 PRINT 3*4',
            'RUN',
        ]
        print(f"Running batch commands: {commands}")
        output = run_batch(commands)
        print("\n--- Output ---")
        print(output)
        print("--- End ---")

        # Simple verification
        if "HELLO WORLD" in output and "4" in output and "TEST" in output and "12" in output:
            print("\nSUCCESS: MS-BASIC is working!")
            return 0
        else:
            print("\nFAILURE: Expected output not found")
            return 1
    else:
        # Interactive mode
        run_interactive()

    return 0

if __name__ == '__main__':
    sys.exit(main())
