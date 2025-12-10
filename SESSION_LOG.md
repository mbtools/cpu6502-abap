# Session Log: MS-BASIC Console HTML Space Fix
**Date:** 2025-12-10

## Summary
Fixed character output display issue in ZCPU6502_CONSOLE where spaces were being collapsed in HTML output, causing "MEMORYSIZE?" instead of "MEMORY SIZE?".

## Problem
- Console displayed text without spaces: "MEMORYSIZE?", "TERMINALWIDTH?", "9231BYTESFREE"
- Root cause: HTML collapses multiple spaces into single space
- Multiple fix attempts on character conversion in `zcl_cpu_00_bus_basic` didn't solve it
- Issue was in HTML rendering, not character conversion

## Solution Applied

### 1. HTML Display Fix (ZCPU6502_CONSOLE)
Changed `lcl_html_display` class:

**escape_html method** - Added space to `&nbsp;` conversion:
```abap
METHOD escape_html.
  rv_text = iv_text.
  REPLACE ALL OCCURRENCES OF '&' IN rv_text WITH '&amp;'.
  REPLACE ALL OCCURRENCES OF '<' IN rv_text WITH '&lt;'.
  REPLACE ALL OCCURRENCES OF '>' IN rv_text WITH '&gt;'.
  REPLACE ALL OCCURRENCES OF '"' IN rv_text WITH '&quot;'.
  " Convert spaces to &nbsp; to preserve multiple spaces
  REPLACE ALL OCCURRENCES OF ` ` IN rv_text WITH '&nbsp;'.
ENDMETHOD.
```

**build_html method** - Changed from `<div>` to `<pre>` tag:
```abap
rv_html =
  |<html><head><style>| &&
  |body \{ background-color: #000000; color: #00ff00; | &&
  |font-family: 'Courier New', monospace; font-size: { gc_font_size }px; | &&
  |padding: 10px; margin: 0; \}| &&
  |pre \{ margin: 0; white-space: pre-wrap; word-wrap: break-word; | &&
  |line-height: { gc_line_height }; \}| &&
  |</style></head><body>| &&
  |<pre>{ lv_content }</pre>| &&
  |</body></html>|.
```

### 2. Default ROM Value
- Added constant: `gc_default_rom TYPE string VALUE 'ZMSBASIC.BIN'`
- Parameter default: `p_rom TYPE wwwdatatab-objid DEFAULT 'ZMSBASIC.BIN'`

### 3. Character Conversion (zcl_cpu_00_bus_basic)
Improved ASCII lookup table approach (though HTML fix was the real solution):
```abap
" I/O: Output character
IF iv_addr = c_io_charout.
  " Handle control characters first
  IF lv_val = 10 OR lv_val = 13.  " LF or CR -> newline
    mv_output_buf = mv_output_buf && cl_abap_char_utilities=>newline.
    RETURN.
  ELSEIF lv_val < 32.  " Other control chars - ignore
    RETURN.
  ENDIF.

  " Printable ASCII characters (32-126)
  DATA: lv_char TYPE c LENGTH 1,
        lv_off  TYPE i.

  CONSTANTS: lc_ascii_32_63  TYPE string VALUE ` !"#$%&'()*+,-./0123456789:;<=>?`,
             lc_ascii_64_95  TYPE string VALUE `@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`,
             lc_ascii_96_126 TYPE string VALUE '`abcdefghijklmnopqrstuvwxyz{|}~'.

  IF lv_val >= 32 AND lv_val <= 63.
    lv_off = lv_val - 32.
    lv_char = lc_ascii_32_63+lv_off(1).
  ELSEIF lv_val >= 64 AND lv_val <= 95.
    lv_off = lv_val - 64.
    lv_char = lc_ascii_64_95+lv_off(1).
  ELSEIF lv_val >= 96 AND lv_val <= 126.
    lv_off = lv_val - 96.
    lv_char = lc_ascii_96_126+lv_off(1).
  ELSE.
    lv_char = '?'.
  ENDIF.

  mv_output_buf = mv_output_buf && lv_char.
  RETURN.
ENDIF.
```

## Files Modified

| Object | Type | Changes |
|--------|------|---------|
| ZCPU6502_CONSOLE | PROG | HTML space fix (`&nbsp;`), `<pre>` tag, default ROM |
| ZCL_CPU_00_BUS_BASIC | CLAS | ASCII lookup table for character conversion |

## Expected Output After Fix
```
6502 Emulator - MS-BASIC
ROM: 30464 bytes
--------------------------------

MEMORY SIZE?

TERMINAL WIDTH?

9231 BYTES FREE

COPYRIGHT 1977 BY MICROSOFT CO.

OK
```

## Key Learnings

1. **HTML whitespace handling**: Browsers collapse multiple spaces into one. Use `&nbsp;` or `<pre>` tags to preserve spaces.

2. **ABAP string constants with backtick**: The backtick character (`) cannot be included in a backtick-delimited string. Use single quotes for strings containing backticks: `'`abc...'`

3. **Input handling**: The `provide_input` method in `zcl_cpu_00_bus_basic` already appends CR to input - no changes needed there.

## Test Commands
```
SE38 -> ZCPU6502_CONSOLE -> Execute
- Select ZMSBASIC.BIN from dropdown (should be default)
- Press Enter for MEMORY SIZE? (accept default)
- Press Enter for TERMINAL WIDTH? (accept default)
- Type: PRINT "HELLO WORLD"
- Type: 10 PRINT "TEST"
- Type: RUN
```

## Related Objects
- `ZCL_CPU_00_SPEEDRUN_BASIC` - Batch test runner
- `ZCL_CPU_00_BASIC_TEST` - Test configuration (uses ZMSBASIC.BIN)
- `ZCPU6502_BASIC_TEST` - Test report
- `ZCPU6502_BASIC_DIAG` - Diagnostic report

## Previous Session Context
This session continued from a previous conversation where:
1. MS-BASIC startup was fixed (use `mo_cpu->provide_input()` not bus method)
2. Added initial `mo_cpu->run()` before command loop
3. Created unit tests in `ZCL_CPU_00_SPEEDRUN_TEST`
4. Complete rewrite of `ZCPU6502_CONSOLE` to match `ZORK_01_CONSOLE` pattern
