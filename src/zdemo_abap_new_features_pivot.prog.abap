*&---------------------------------------------------------------------*
*& Report  ZDEMO_ABAP_NEW_FEATURES_PIVOT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdemo_abap_new_features_pivot.

*----------------------------------------------------------------------*
*       CLASS lcl_ABAP_NEW_FEATURES DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_new_features DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.                    "lcl_ABAP_NEW_FEATURES DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_text_file DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_text_file DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_po_data,
        ebeln  TYPE c LENGTH 10,
        bsart TYPE c LENGTH 4,
        aedat  TYPE c LENGTH 10,
        lifnr TYPE c LENGTH 10,
        bedat TYPE c LENGTH 10,
      END OF ty_s_po_data.
    TYPES: ty_t_po_data TYPE STANDARD TABLE OF ty_s_po_data
                     WITH NON-UNIQUE KEY ebeln.

    METHODS constructor IMPORTING im_path TYPE string OPTIONAL.
    METHODS upload_file IMPORTING im_path TYPE string OPTIONAL
                        RETURNING value(result_ok) TYPE abap_bool.
    METHODS get_po_data RETURNING value(value) TYPE ty_t_po_data.
  PRIVATE SECTION.
    DATA: lv_path TYPE string.

    DATA: lt_po_data TYPE ty_t_po_data.
ENDCLASS.                    "lcl_text_file DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_data DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data_model DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_text_file TYPE REF TO lcl_text_file.
    METHODS create_pivot_table RETURNING value(pivot_tab) TYPE REF TO data.
  PRIVATE SECTION.

    DATA: lo_text_file TYPE REF TO lcl_text_file.
    DATA: lo_structure_type  TYPE REF TO  cl_abap_structdescr,
          lo_table_type  TYPE REF TO  cl_abap_tabledescr.
    DATA: lo_table TYPE REF TO data,
          lo_structure TYPE REF TO data.

    METHODS create_dynamic_table_type.
    METHODS collect_data.
ENDCLASS.                    "lcl_data_model DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_salv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_salv DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_data TYPE REF TO data.
    METHODS show_report.
  PRIVATE SECTION.
    DATA: lo_pivot_table TYPE REF TO data.
    DATA: lo_salv TYPE REF TO cl_salv_table.

    METHODS update_fieldcat.
ENDCLASS.                    "lcl_salv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_control DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_control DEFINITION.
  PUBLIC SECTION.
    METHODS create_pivot_table
            IMPORTING io_text_file TYPE REF TO lcl_text_file.
    METHODS show_report.
  PRIVATE SECTION.
    DATA: lo_data_model TYPE REF TO lcl_data_model.
    DATA: lo_salv TYPE REF TO lcl_salv.
    DATA: lo_pivot_table TYPE REF TO data.
ENDCLASS.                    "lcl_control DEFINITION

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Start application
  lcl_abap_new_features=>main( ).

*----------------------------------------------------------------------*
*       CLASS lcl_ABAP_NEW_FEATURES IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_new_features IMPLEMENTATION.
  METHOD main.
    DATA: lo_text_file TYPE REF TO lcl_text_file.

* Declare and create an instant of class LCL_CONTROL
    DATA(lo_control) = NEW lcl_control( ).

* Create text file object
    lo_text_file = NEW #( im_path = 'C:\Users\Wises\Desktop\Training\PO_HEADER_LIST.txt').

* Upload file
    IF abap_true = lo_text_file->upload_file( ).

* Process data model
      lo_control->create_pivot_table( lo_text_file ).

* Show report
      lo_control->show_report( ).
    ELSE.
      MESSAGE |Fialed to upload input file| TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.                    "main
ENDCLASS.                    "lcl_ABAP_NEW_FEATURES IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_text_file IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_text_file IMPLEMENTATION.
  METHOD constructor.
* File path
    IF im_path IS SUPPLIED.
      lv_path = im_path.
    ENDIF.

    CLEAR: lt_po_data.
  ENDMETHOD.                    "constructor
  METHOD upload_file.

    IF im_path IS SUPPLIED.
      lv_path = im_path.
    ENDIF.

* Check path
    IF lv_path IS INITIAL.
      MESSAGE 'Specify path to be uploaded' TYPE 'S'.
    ELSE.
      CLEAR lt_po_data.

      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = lv_path
          filetype                = 'ASC'
          has_field_separator     = cl_abap_char_utilities=>horizontal_tab
        CHANGING
          data_tab                = lt_po_data
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19.
      IF sy-subrc = 0.
        result_ok = abap_true.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE sy-msgty
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "upload_file
  METHOD get_po_data.
    value = lt_po_data.
  ENDMETHOD.                    "get_po_data
ENDCLASS.                    "lcl_text_file IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_data IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data_model IMPLEMENTATION.
  METHOD constructor.
    lo_text_file = io_text_file.
  ENDMETHOD.                    "constructor
  METHOD create_pivot_table.
* Create table type
    me->create_dynamic_table_type( ).
* Collect data
    me->collect_data( ).
* Return output table
    IF pivot_tab IS SUPPLIED AND lo_table IS NOT INITIAL.
      pivot_tab = lo_table.
    ENDIF.
  ENDMETHOD.                    "create_pivot_table
  METHOD create_dynamic_table_type.
    TYPES:
      BEGIN OF ty_my,
        year_month TYPE c LENGTH 7,
        month_year TYPE c LENGTH 7,
      END OF ty_my.

    DATA: lo_element TYPE REF TO cl_abap_elemdescr.

    DATA: lt_comps TYPE cl_abap_structdescr=>component_table,
          ls_comp TYPE LINE OF cl_abap_structdescr=>component_table.

    DATA: lt_my TYPE STANDARD TABLE OF ty_my,
          ls_my TYPE ty_my.

* Vendor
    ls_comp-name = 'LIFNR'.
    lo_element ?= cl_abap_elemdescr=>describe_by_name( 'CHAR10' ).
    ls_comp-type = cl_abap_elemdescr=>get_c( p_length = lo_element->length ).
    APPEND ls_comp TO lt_comps.
    CLEAR ls_comp.
    FREE: lo_element.

* PO data
    DATA(lt_po_data) = lo_text_file->get_po_data( ).

    LOOP AT lt_po_data INTO DATA(ls_po_data).
      ls_my-year_month = |F{ ls_po_data-bedat+6(4) }{ ls_po_data-bedat+3(2) }|.
      ls_my-month_year = |{ ls_po_data-bedat+3(2) }/{ ls_po_data-bedat+6(4) }|.

      COLLECT ls_my INTO lt_my.
    ENDLOOP.

    SORT lt_my BY year_month.
    LOOP AT lt_my INTO ls_my.
      ls_comp-name = ls_my-year_month.
      ls_comp-type = cl_abap_elemdescr=>get_i( ).
      APPEND ls_comp TO lt_comps.
      CLEAR ls_comp.
      FREE: lo_element.
    ENDLOOP.

* Create a Structure type
    lo_structure_type = cl_abap_structdescr=>create( lt_comps ).

* Create Table type
    lo_table_type = cl_abap_tabledescr=>create(
                    p_line_type  = lo_structure_type
                    p_table_kind = cl_abap_tabledescr=>tablekind_std
                    p_unique     = abap_false ).

* Create data
    CREATE DATA lo_structure TYPE HANDLE lo_structure_type.
    CREATE DATA lo_table TYPE HANDLE lo_table_type.
  ENDMETHOD.                    "create_dynamic_table_type
  METHOD collect_data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    DATA: lt_po_data TYPE lcl_text_file=>ty_t_po_data,
          ls_po_data TYPE lcl_text_file=>ty_s_po_data.

    DATA: lv_field TYPE string.
    FIELD-SYMBOLS <field> TYPE ANY.

* Check data object
    CHECK lo_structure IS NOT INITIAL AND lo_table IS NOT INITIAL.

* PO data
    lt_po_data = lo_text_file->get_po_data( ).

    ASSIGN lo_structure->* TO FIELD-SYMBOL(<struct>).
    IF sy-subrc = 0.
      ASSIGN lo_table->* TO <table>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.

    LOOP AT lt_po_data INTO ls_po_data.
* Vendor number
      lv_field = '<STRUCT>-LIFNR'.
      ASSIGN (lv_field) TO <field>.
      IF sy-subrc = 0.
        <field> = ls_po_data-lifnr.
      ENDIF.
      CLEAR lv_field.

* Counter
      lv_field = |<STRUCT>-F{ ls_po_data-bedat+6(4) }{ ls_po_data-bedat+3(2) }|.
      ASSIGN (lv_field) TO <field>.
      IF sy-subrc = 0.
        <field> = 1.
      ENDIF.
      CLEAR lv_field.

      COLLECT <struct> INTO <table>.
      CLEAR <struct>.
    ENDLOOP.
  ENDMETHOD.                    "collect_data
ENDCLASS.                    "lcl_data_model IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS salv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_salv IMPLEMENTATION.
  METHOD constructor.
    DATA: ls_key TYPE salv_s_layout_key.

    FIELD-SYMBOLS: <pivot_table> TYPE STANDARD TABLE.

* Data object
    lo_pivot_table = io_data.

* Table
    ASSIGN lo_pivot_table->* TO <pivot_table>.
    IF sy-subrc = 0.
* Factory SALV
      TRY .
          cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv
                                  CHANGING t_table = <pivot_table> ).

* Function
          DATA(lo_functions) = lo_salv->get_functions( ).
          lo_functions->set_all( abap_true ).

* Layout
          DATA(lo_layout) = lo_salv->get_layout( ).
          ls_key-report = sy-cprog.
          lo_layout->set_key( ls_key ).
          lo_layout->set_save_restriction(
                     if_salv_c_layout=>restrict_none ).

* Field catalog
          me->update_fieldcat( ).

        CATCH cx_salv_msg INTO DATA(lx_msg).

      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "constructor
  METHOD show_report.
    IF lo_salv IS NOT INITIAL.
      lo_salv->display( ).
    ELSE.
      MESSAGE 'Could not create an ALV object' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.                    "show_report
  METHOD update_fieldcat.
    DATA: lv_col_text TYPE string.

* Get all columns
    DATA(lo_cols) = lo_salv->get_columns( ).

    DATA(lt_cols) = lo_cols->get( ).
    LOOP AT lt_cols INTO DATA(ls_cols).
      IF ls_cols-columnname = 'LIFNR'.
        lv_col_text = 'Vendor Number'.

        ls_cols-r_column->set_output_length( 13 ).
        ls_cols-r_column->set_short_text( CONV #( lv_col_text ) ).
        ls_cols-r_column->set_medium_text( CONV #( lv_col_text ) ).
        ls_cols-r_column->set_long_text( CONV #( lv_col_text ) ).
      ELSE.
        lv_col_text = |{ ls_cols-columnname+5(2) }/{ ls_cols-columnname+1(4) }|.
        ls_cols-r_column->set_short_text( CONV #( lv_col_text ) ).
        ls_cols-r_column->set_medium_text( CONV #( lv_col_text ) ).
        ls_cols-r_column->set_long_text( CONV #( lv_col_text ) ).

        ls_cols-r_column->set_zero( abap_false ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "update_fieldcat
ENDCLASS.                    "salv IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_control IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_control IMPLEMENTATION.
  METHOD create_pivot_table.

* Create data object
    lo_data_model = NEW #( io_text_file = io_text_file ).

* Create Pivot table
    lo_pivot_table = lo_data_model->create_pivot_table( ).

  ENDMETHOD.                    "create_pivot_table
  METHOD show_report.

    IF lo_pivot_table IS NOT INITIAL.
* Create SALV object
      lo_salv = NEW #( io_data = lo_pivot_table ).

* Show report
      lo_salv->show_report( ).
    ENDIF.

  ENDMETHOD.                    "show_report
ENDCLASS.                    "lcl_control IMPLEMENTATION
