*&---------------------------------------------------------------------*
*& Report ZCPU6502_CONSOLE
*& 6502 CPU Emulator - Interactive Console
*&---------------------------------------------------------------------*
REPORT zcpu6502_console.

TABLES sscrfields.

DATA: go_bus TYPE REF TO zcl_cpu_00_bus_simple,
      go_cpu TYPE REF TO zcl_cpu_00_cpu,
      gv_initialized TYPE abap_bool.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE string LOWER CASE DEFAULT '/tmp/test.bin',
              p_addr TYPE i DEFAULT 32768.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_input TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  sscrfields-functxt_01 = 'Run Step'.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    PERFORM run_step.
  ENDIF.

START-OF-SELECTION.
  PERFORM main.

FORM main.
  DATA: lv_rom    TYPE xstring,
        lv_output TYPE string,
        ls_status TYPE zcl_cpu_00_cpu=>ts_status.

  DATA(lo_loader) = NEW zcl_cpu_00_rom_loader_file( ).
  lv_rom = lo_loader->zif_cpu_00_rom_loader~load( p_file ).

  IF lv_rom IS INITIAL.
    WRITE: / 'Failed to load ROM file:', p_file.
    RETURN.
  ENDIF.

  go_bus = NEW zcl_cpu_00_bus_simple( ).
  go_bus->zif_cpu_00_bus~load( iv_addr = p_addr iv_data = lv_rom ).
  go_cpu = NEW zcl_cpu_00_cpu( go_bus ).
  gv_initialized = abap_true.

  WRITE: / '6502 CPU Emulator'.
  WRITE: / 'ROM loaded:', xstrlen( lv_rom ), 'bytes at $', p_addr.
  ULINE.

  go_cpu->run( iv_max_cycles = 1000000 ).
  lv_output = go_bus->zif_cpu_00_bus~get_output( ).
  PERFORM display_output USING lv_output.

  ls_status = go_cpu->get_status( ).
  WRITE: /.
  WRITE: / 'Status: A=', ls_status-a, ' X=', ls_status-x, ' Y=', ls_status-y.
  WRITE: / '        PC=$', ls_status-pc, ' SP=$', ls_status-sp.
  WRITE: / '        Cycles:', ls_status-cycles.

  IF ls_status-waiting = abap_true.
    WRITE: / 'Waiting for input...'.
  ELSEIF ls_status-running = abap_false.
    WRITE: / 'CPU halted.'.
  ENDIF.
ENDFORM.

FORM run_step.
  DATA: lv_output TYPE string,
        ls_status TYPE zcl_cpu_00_cpu=>ts_status.

  IF gv_initialized = abap_false OR go_cpu IS INITIAL.
    MESSAGE 'Please run the program first' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_input IS NOT INITIAL.
    go_cpu->provide_input( p_input ).
    CLEAR p_input.
  ENDIF.

  go_bus->zif_cpu_00_bus~clear_output( ).
  go_cpu->run( iv_max_cycles = 1000000 ).

  lv_output = go_bus->zif_cpu_00_bus~get_output( ).
  IF lv_output IS NOT INITIAL.
    MESSAGE lv_output TYPE 'I'.
  ENDIF.
ENDFORM.

FORM display_output USING iv_output TYPE string.
  DATA: lt_lines TYPE STANDARD TABLE OF string,
        lv_line  TYPE string.

  IF iv_output IS INITIAL.
    RETURN.
  ENDIF.

  SPLIT iv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
  LOOP AT lt_lines INTO lv_line.
    WRITE: / lv_line.
  ENDLOOP.
ENDFORM.