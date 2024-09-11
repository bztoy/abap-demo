CLASS zcl_geometric_shape DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES ty_area TYPE p LENGTH 8 DECIMALS 2.
    METHODS constructor IMPORTING i_name TYPE string.
    METHODS cal_area ABSTRACT RETURNING VALUE(value) TYPE ty_area.
    METHODS print_shape_info FINAL.
    METHODS get_shape_info RETURNING VALUE(value) TYPE string.
  PROTECTED SECTION.
    DATA shape_area TYPE ty_area.
  PRIVATE SECTION.
    DATA shape_type TYPE string.
ENDCLASS.



CLASS zcl_geometric_shape IMPLEMENTATION.
  METHOD constructor.
    shape_type = i_name.
  ENDMETHOD.

  METHOD print_shape_info.
    cl_demo_output=>display(
    EXPORTING
        data = |Shape Type is { shape_type }| &
            | and it's area is { shape_area }|
    ).
  ENDMETHOD.

  METHOD get_shape_info.
    value = |Shape Type is { shape_type }| &
            | and it's area is { shape_area }|.
  ENDMETHOD.

ENDCLASS.
