CLASS zcl_cpu_00_script_loader_file DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_cpu_00_script_loader.

    METHODS constructor
      IMPORTING iv_filepath TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_filepath TYPE string.

ENDCLASS.



CLASS zcl_cpu_00_script_loader_file IMPLEMENTATION.

  METHOD constructor.
    mv_filepath = iv_filepath.
  ENDMETHOD.

  METHOD zif_cpu_00_script_loader~load.
    DATA: lv_content TYPE string,
          lv_line    TYPE string.

    IF mv_filepath IS INITIAL.
      RETURN.
    ENDIF.

    OPEN DATASET mv_filepath FOR INPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc = 0.
      DO.
        READ DATASET mv_filepath INTO lv_line.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        APPEND lv_line TO rt_commands.
      ENDDO.
      CLOSE DATASET mv_filepath.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
