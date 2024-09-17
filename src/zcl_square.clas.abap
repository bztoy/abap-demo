CLASS zcl_square DEFINITION
  PUBLIC
  INHERITING FROM zcl_geometric_shape
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  METHODS constructor IMPORTING i_width TYPE i.
  METHODS: cal_area REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: _width TYPE i.
    DATA: dummy_value TYPE i VALUE 0.
ENDCLASS.



CLASS zcl_square IMPLEMENTATION.
  METHOD cal_area.
    shape_area = _width ** 2.

    IF value IS SUPPLIED.
    value = shape_area.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( 'SQUARE' ).

    _width = i_width.
  ENDMETHOD.
ENDCLASS.
