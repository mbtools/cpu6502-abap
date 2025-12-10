*&---------------------------------------------------------------------*
*& Report ZCPU6502_SPEEDRUN
*& 6502 CPU Emulator - Automated Test Runner
*&---------------------------------------------------------------------*
REPORT zcpu6502_speedrun.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_rom    TYPE string LOWER CASE DEFAULT '/tmp/test.bin',
              p_addr   TYPE i DEFAULT 32768,
              p_script TYPE string LOWER CASE DEFAULT '/tmp/test.txt'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM main.

FORM main.
  DATA: lv_rom      TYPE xstring,
        lt_commands TYPE zif_cpu_00_script_loader=>tt_commands,
        ls_result   TYPE zcl_cpu_00_speedrun=>ts_result,
        lt_log      TYPE zcl_cpu_00_speedrun=>tt_log,
        ls_log      TYPE zcl_cpu_00_speedrun=>ts_log_entry.

  DATA(lo_rom_loader) = NEW zcl_cpu_00_rom_loader_file( ).
  lv_rom = lo_rom_loader->zif_cpu_00_rom_loader~load( p_rom ).

  IF lv_rom IS INITIAL.
    WRITE: / 'ERROR: Failed to load ROM file:', p_rom.
    RETURN.
  ENDIF.

  DATA(lo_script_loader) = NEW zcl_cpu_00_script_loader_file( p_script ).
  lt_commands = lo_script_loader->zif_cpu_00_script_loader~load( ).

  IF lines( lt_commands ) = 0.
    WRITE: / 'ERROR: Failed to load script file:', p_script.
    RETURN.
  ENDIF.

  WRITE: / '6502 Speedrun Test'.
  WRITE: / 'ROM:', p_rom, '(', xstrlen( lv_rom ), 'bytes )'.
  WRITE: / 'Script:', p_script, '(', lines( lt_commands ), 'lines )'.
  ULINE.

  DATA(lo_speedrun) = NEW zcl_cpu_00_speedrun(
    iv_rom      = lv_rom
    iv_loadaddr = p_addr
    it_commands = lt_commands ).

  ls_result = lo_speedrun->run( ).
  lt_log = lo_speedrun->get_log( ).

  WRITE: / 'Execution Log:'.
  WRITE: / '---'.
  LOOP AT lt_log INTO ls_log.
    CASE ls_log-role.
      WHEN 'I'.
        WRITE: / '>>>', ls_log-text.
      WHEN 'O'.
        WRITE: / ls_log-text.
      WHEN '+'.
        WRITE: / '[PASS]', ls_log-text COLOR COL_POSITIVE.
      WHEN '-'.
        WRITE: / '[FAIL]', ls_log-text COLOR COL_NEGATIVE.
      WHEN 'S'.
        WRITE: / '[INFO]', ls_log-text COLOR COL_TOTAL.
      WHEN 'E'.
        WRITE: / '[ERROR]', ls_log-text COLOR COL_NEGATIVE.
    ENDCASE.
  ENDLOOP.

  ULINE.
  WRITE: / 'Results:'.
  WRITE: / '  Commands:', ls_result-commands_run, '/', ls_result-commands_total.
  WRITE: / '  Assertions:', ls_result-assertions_pass, '/', ls_result-assertions_total, 'passed'.

  IF ls_result-success = abap_true.
    WRITE: / 'TEST PASSED' COLOR COL_POSITIVE.
  ELSE.
    WRITE: / 'TEST FAILED' COLOR COL_NEGATIVE.
    WRITE: / '  Failures:', ls_result-assertions_fail.
  ENDIF.

  IF ls_result-cpu_halted = abap_true.
    WRITE: / '  (CPU halted during execution)'.
  ENDIF.
ENDFORM.