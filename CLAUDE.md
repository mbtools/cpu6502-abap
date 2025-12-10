# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CPU6502-ABAP is a MOS 6502 CPU emulator written in ABAP. It executes 6502 machine code on SAP systems.

**Status:** Working! All 44 unit tests pass.

## Development Environment

### MCP Server (vibing-steampunk / vsp)

This project uses [vibing-steampunk](https://github.com/oisee/vibing-steampunk) (`a4h-abap-adt` MCP server) for SAP ADT access. Key MCP tools:

| Tool | Purpose |
|------|---------|
| `SearchObject` | Find ABAP objects (`SearchObject(query="ZCL_CPU_00*")`) |
| `GetSource` | Read source code (`GetSource(object_type="CLAS", name="ZCL_CPU_00_CPU")`) |
| `WriteSource` | Create/update source (`WriteSource(object_type="CLAS", name="...", source="...")`) |
| `EditSource` | Surgical edits (`EditSource(object_url="...", old_string="...", new_string="...")`) |
| `RunUnitTests` | Execute tests (`RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_CPU_00_TEST")`) |
| `Activate` | Activate objects |
| `SyntaxCheck` | Validate syntax before activation |

### SAP Resources

- **Package:** `$CPU6502`

## Architecture

### Core CPU Components (src/cpu6502/)

| Class | Purpose |
|-------|---------|
| `zcl_cpu_00_cpu` | Main CPU emulator: registers, fetch-decode-execute cycle |
| `zcl_cpu_00_bus_simple` | Simple 64KB RAM bus implementation |
| `zcl_cpu_00_test` | Unit tests (44 tests covering all operations) |
| `zcl_cpu_00_speedrun` | Automated test execution |

### Interfaces

| Interface | Purpose |
|-----------|---------|
| `zif_cpu_00_bus` | Bus interface for memory read/write |
| `zif_cpu_00_rom_loader` | Interface for ROM file loading |
| `zif_cpu_00_script_loader` | Interface for test scripts |

### Entry Points (Programs)

| Program | Purpose |
|---------|---------|
| `zcpu6502_console` | Interactive console for stepping through code |
| `zcpu6502_speedrun` | Batch execution with verification |

## Running Tests

```
RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_CPU_00_TEST")
```

All 44 tests cover:
- Arithmetic (ADC, SBC with carry/overflow)
- Logical (AND, ORA, EOR)
- Shifts/Rotates (ASL, LSR, ROL, ROR)
- Comparisons (CMP, CPX, CPY)
- Branches (BEQ, BNE, BCS, BCC, BMI, BPL)
- Jumps (JMP, JSR, RTS)
- Stack (PHA, PLA, PHP, PLP)
- All addressing modes

## Critical Implementation Details

### Integer Division (CRITICAL!)

ABAP's `/` operator performs **decimal** division, not integer division. All bit operations MUST use `DIV`:

```abap
" CORRECT - integer division
lv_result = mv_a DIV 2.

" WRONG - decimal division (will break bit operations!)
lv_result = mv_a / 2.
```

This affects:
- LSR (Logical Shift Right): `value DIV 2`
- ROR (Rotate Right): `value DIV 2`
- Address calculations: `addr DIV 256`

### Status Flags

```abap
" Flag positions in P register
" N V - B D I Z C
" 7 6 5 4 3 2 1 0

" Setting flags
IF lv_result = 0.
  mv_p = mv_p BIT-OR 2.    " Set Z flag
ELSE.
  mv_p = mv_p BIT-AND 253. " Clear Z flag
ENDIF.

IF lv_result >= 128.
  mv_p = mv_p BIT-OR 128.  " Set N flag
ELSE.
  mv_p = mv_p BIT-AND 127. " Clear N flag
ENDIF.
```

### Memory Access

```abap
" Read byte from bus
lv_value = mo_bus->read( iv_addr ).

" Write byte to bus
mo_bus->write( iv_addr = lv_addr iv_value = lv_value ).

" 16-bit address (little-endian)
lv_lo = mo_bus->read( iv_addr ).
lv_hi = mo_bus->read( iv_addr + 1 ).
lv_addr16 = lv_hi * 256 + lv_lo.
```

### Stack Operations

```abap
" Stack is at page $01 (addresses $0100-$01FF)
" SP points to next free location, grows downward

" Push
mo_bus->write( iv_addr = 256 + mv_sp iv_value = lv_value ).
mv_sp = mv_sp - 1.
IF mv_sp < 0. mv_sp = 255. ENDIF.

" Pull
mv_sp = mv_sp + 1.
IF mv_sp > 255. mv_sp = 0. ENDIF.
lv_value = mo_bus->read( 256 + mv_sp ).
```

## ABAP Naming Conventions

| Prefix | Usage | Example |
|--------|-------|---------|
| `ts_` | Structure types | `ts_instruction` |
| `tt_` | Table types | `tt_memory` |
| `lv_` | Local variables | `lv_opcode` |
| `lt_` | Local tables | `lt_bytes` |
| `mv_` | Instance attributes | `mv_pc`, `mv_a`, `mv_x` |
| `mt_` | Instance tables | `mt_memory` |
| `mo_` | Instance objects | `mo_bus` |
| `iv_` | Import parameters | `iv_addr` |
| `ev_` | Export parameters | `ev_value` |
| `rv_` | Return values | `rv_result` |

**Note:** Use `ts_` for structures, NOT `ty_`.

## File Naming (abapGit)

- `.clas.abap` - Class main source
- `.clas.testclasses.abap` - Unit test classes
- `.intf.abap` - Interfaces
- `.prog.abap` - Reports/programs

## 6502 Opcode Quick Reference

### Load/Store
`LDA`, `LDX`, `LDY`, `STA`, `STX`, `STY`

### Transfer
`TAX`, `TXA`, `TAY`, `TYA`, `TSX`, `TXS`

### Stack
`PHA`, `PLA`, `PHP`, `PLP`

### Arithmetic
`ADC`, `SBC`, `INC`, `DEC`, `INX`, `DEX`, `INY`, `DEY`

### Logical
`AND`, `ORA`, `EOR`

### Shift/Rotate
`ASL`, `LSR`, `ROL`, `ROR`

### Compare
`CMP`, `CPX`, `CPY`, `BIT`

### Branch
`BEQ`, `BNE`, `BCS`, `BCC`, `BMI`, `BPL`, `BVS`, `BVC`

### Jump/Call
`JMP`, `JSR`, `RTS`, `RTI`, `BRK`

### Flags
`CLC`, `SEC`, `CLI`, `SEI`, `CLV`, `CLD`, `SED`

### No-op
`NOP`
