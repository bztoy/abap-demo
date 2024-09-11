CLASS zcl_round DEFINITION
  PUBLIC
  INHERITING FROM zcl_geometric_shape
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_rad TYPE i.
    METHODS: cal_area REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: rad TYPE i.

    CONSTANTS pi TYPE p LENGTH 3 DECIMALS 2 VALUE '3.14'.
ENDCLASS.



CLASS zcl_round IMPLEMENTATION.
  METHOD constructor.

    super->constructor( 'ROUND' ).

    rad = i_rad.
  ENDMETHOD.
  METHOD cal_area.
    shape_area = 2 * pi * rad.

    IF value IS SUPPLIED.
      value = shape_area.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
