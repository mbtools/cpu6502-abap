# Questions about MS-BASIC Build for ABAP 6502 Emulator

These questions are for the colleague who built the MS-BASIC binary (ZMSBASIC.BIN) to help sync the ABAP emulator implementation with the ROM configuration.

---

## 1. REALIO Configuration

- What **REALIO value** was used for the ABAP target build?
- Looking at the original code, `REALIO=4` is Apple, which uses `JSR CQINLN` for line input. Was a custom REALIO value created for ABAP?
- Which I/O model: character-at-a-time or line-buffered?

## 2. Input Echo Behavior

- Does MS-BASIC **echo input characters itself**, or does it expect the host to echo?
- In the source (`m6502.asm` lines 1703, 1739), echo is done via `JSR OUTDO` but this is conditional on REALIO value
- For Apple (`REALIO=4`), it delegates to `CQINLN` which presumably handles echo externally
- **Current behavior**: ABAP emulator does NOT echo input - should it?

## 3. I/O Addresses

The ABAP bus implementation uses:

| Address | Name | Function |
|---------|------|----------|
| `$FFF0` | CHAROUT | Write character to output |
| `$FFF1` | CHARIN | Read character from input (consuming) |
| `$FFF2` | STATUS | I/O status (bit 0 = char available) |
| `$FFF3` | PEEK | Peek next char without consuming |

**Questions:**
- Are these the correct addresses for this build?
- What addresses were patched/configured in the ROM?
- Does the ROM poll STATUS before reading CHARIN?

## 4. Line Input vs Character Input

- Does BASIC call `RDKEY` **character-by-character**, or does it expect a **full line**?
- The Python version has a PC hook at `ADDR_RDKEY = 0x28CE` - is this the correct address?
- What is the `CQINLN` address if used?
- Does input routine wait for CR before returning the line?

## 5. Cold Start Address

- Currently using `$2730` (10032 decimal) as cold start
- Is this correct? The Python reference uses `ADDR_COLD_START = 0x2730`
- What is the warm start address?

## 6. CR vs CRLF

- Does BASIC expect **CR (13)**, **LF (10)**, or **CRLF** for line termination?
- Currently ABAP sends just CR after each input line
- What does BASIC output for newlines?

## 7. Build Source

- Was this built from https://github.com/mist64/msbasic with modifications?
- Is there an `abap.cfg` or similar target configuration file?
- What assembler was used (cc65/ca65)?
- Can we get the build configuration/makefile?

## 8. Double Enter Issue

**Symptom**: User sometimes needs to press Enter twice for prompts like "MEMORY SIZE?"

**Possible causes:**
- How STATUS (`$FFF2`) is polled vs CHARIN read?
- Does the input routine clear the buffer after reading?
- Is there a timing/synchronization issue?

---

## Current ABAP Implementation Details

### Bus Class: `zcl_cpu_00_bus_basic`

```abap
" I/O addresses
CONSTANTS: c_io_charout TYPE i VALUE 65520,  " $FFF0
           c_io_charin  TYPE i VALUE 65521,  " $FFF1
           c_io_status  TYPE i VALUE 65522,  " $FFF2
           c_io_peek    TYPE i VALUE 65523.  " $FFF3

" Read from CHARIN - consuming
IF iv_addr = c_io_charin.
  " Return next char from buffer, advance position

" Read from STATUS
IF iv_addr = c_io_status.
  " Return 1 if chars available, 0 otherwise
```

### Input Handling

```abap
" provide_input appends CR to input
METHOD zif_cpu_00_bus~provide_input.
  DATA lv_cr TYPE c LENGTH 1.
  lv_cr = cl_abap_char_utilities=>cr_lf(1).  " Just CR
  mv_input_buf = mv_input_buf && iv_text && lv_cr.
ENDMETHOD.
```

### CPU Waiting State

- CPU sets `mv_waiting = true` when reading from `$FFF1` with no input available
- `cpu->provide_input()` clears waiting flag AND adds to buffer
- `cpu->run()` continues until waiting or max cycles

---

## Contact

Please respond with answers or point to relevant source files/configs. This will help us get MS-BASIC working smoothly on the ABAP 6502 emulator!
