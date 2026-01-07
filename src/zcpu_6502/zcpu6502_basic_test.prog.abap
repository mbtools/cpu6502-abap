*&---------------------------------------------------------------------*
*& Report ZCPU6502_BASIC_TEST
*& Test MS-BASIC on 6502 Emulator
*&---------------------------------------------------------------------*
*& Upload MSBASIC.BIN to SMW0 (Binary data) before running
*&---------------------------------------------------------------------*
REPORT zcpu6502_basic_test.

DATA: go_loader   TYPE REF TO zcl_cpu_00_rom_loader_smw0,
      go_speedrun TYPE REF TO zcl_cpu_00_speedrun_basic,
      gv_rom      TYPE xstring,
      gt_commands TYPE zif_cpu_00_script_loader=>tt_commands,
      gs_result   TYPE zcl_cpu_00_speedrun_basic=>ts_result,
      gt_log      TYPE zcl_cpu_00_speedrun_basic=>tt_log.

START-OF-SELECTION.

  " Load ROM from SMW0
  go_loader = NEW zcl_cpu_00_rom_loader_smw0( iv_pattern = '*BASIC*' ).
  gv_rom = go_loader->zif_cpu_00_rom_loader~load( 'MSBASIC.BIN' ).

  IF gv_rom IS INITIAL.
    WRITE: / 'ERROR: Could not load MSBASIC.BIN from SMW0'.
    WRITE: / 'Please upload bin/msbasic.bin via SMW0 -> Binary data'.
    RETURN.
  ENDIF.

  WRITE: / |Loaded ROM: { xstrlen( gv_rom ) } bytes|.

  " Create test script
  " First two empty lines for MEMORY SIZE and TERMINAL WIDTH prompts
  APPEND '' TO gt_commands.
  APPEND '' TO gt_commands.
  " Now test commands
  APPEND 'PRINT "HELLO WORLD"' TO gt_commands.
  APPEND '%*HELLO WORLD' TO gt_commands.
  APPEND 'PRINT 2+2' TO gt_commands.
  APPEND '%*4' TO gt_commands.
  APPEND '10 PRINT "TEST"' TO gt_commands.
  APPEND '20 PRINT 3*4' TO gt_commands.
  APPEND 'RUN' TO gt_commands.
  APPEND '%*TEST' TO gt_commands.
  APPEND '%*12' TO gt_commands.

  " Run speedrun
  go_speedrun = NEW zcl_cpu_00_speedrun_basic(
    iv_rom      = gv_rom
    it_commands = gt_commands ).

  gs_result = go_speedrun->run( ).
  gt_log = go_speedrun->get_log( ).

  " Display results
  WRITE: / '=== MS-BASIC Test Results ==='.
  SKIP.

  LOOP AT gt_log INTO DATA(ls_log).
    CASE ls_log-role.
      WHEN 'S'.  " Status
        WRITE: / |[STATUS] { ls_log-text }|.
      WHEN 'I'.  " Input
        WRITE: / |[INPUT]  { ls_log-text }|.
      WHEN 'O'.  " Output
        WRITE: / |[OUTPUT] { ls_log-text }|.
      WHEN '+'.  " Pass
        FORMAT COLOR COL_POSITIVE.
        WRITE: / |[PASS]   { ls_log-text }|.
        FORMAT COLOR OFF.
      WHEN '-'.  " Fail
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / |[FAIL]   { ls_log-text }|.
        FORMAT COLOR OFF.
      WHEN 'E'.  " Error
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / |[ERROR]  { ls_log-text }|.
        FORMAT COLOR OFF.
    ENDCASE.
  ENDLOOP.

  SKIP.
  WRITE: / '=== Summary ==='.
  WRITE: / |Commands: { gs_result-commands_run }/{ gs_result-commands_total }|.
  WRITE: / |Assertions: { gs_result-assertions_pass }/{ gs_result-assertions_total }|.

  IF gs_result-success = abap_true.
    FORMAT COLOR COL_POSITIVE.
    WRITE: / 'SUCCESS! MS-BASIC is working!'.
    FORMAT COLOR OFF.
  ELSE.
    FORMAT COLOR COL_NEGATIVE.
    WRITE: / 'FAILURE - some tests failed'.
    FORMAT COLOR OFF.
  ENDIF.