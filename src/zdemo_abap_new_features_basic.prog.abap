*&---------------------------------------------------------------------*
*& Report zdemo_abap_new_features_basic
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_abap_new_features_basic.

CLASS lcl_new_features_basic DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS inline_declarations_and_itab.
    CLASS-METHODS internal_table.
    CLASS-METHODS conv_and_cast.
    CLASS-METHODS value_and_new_commands.
    CLASS-METHODS ref_command.
ENDCLASS.

CLASS lcl_post_goods_mvt DEFINITION.
  PUBLIC SECTION.
    METHODS post IMPORTING i_bwart TYPE bwart.
ENDCLASS.

CLASS lcl_post_goods_mvt IMPLEMENTATION.

  METHOD post.
*    MESSAGE |Movement type to post is { i_bwart }| TYPE 'S'.
    cl_demo_output=>display(
      |Movement type to post is { i_bwart }| ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_bird DEFINITION.
  PUBLIC SECTION.
    METHODS speak IMPORTING i_sound TYPE string OPTIONAL.
  PRIVATE SECTION.
    CONSTANTS: lc_default_sound TYPE string VALUE 'Tweet'.
ENDCLASS.

CLASS lcl_bird IMPLEMENTATION.

  METHOD speak.
    cl_demo_output=>display( COND #( WHEN i_sound IS INITIAL THEN lc_default_sound
                                     ELSE
                                       i_sound
                                    )
                            ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_box DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_dimension,
        w TYPE i,
        l TYPE i,
        h TYPE i,
      END OF ty_dimension.

    METHODS constructor IMPORTING p_dms TYPE ty_dimension.
    METHODS get_boxsize RETURNING VALUE(value) TYPE i.
  PRIVATE SECTION.
    DATA: box_size TYPE i.
ENDCLASS.

CLASS lcl_box IMPLEMENTATION.
  METHOD constructor.
    box_size = p_dms-w * p_dms-l * p_dms-h.
  ENDMETHOD.
  METHOD get_boxsize.
    value = box_size.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  lcl_new_features_basic=>main( ).

CLASS lcl_new_features_basic IMPLEMENTATION.
  METHOD main.
* In-line Declaration
*    lcl_new_features_basic=>inline_declarations_and_itab( ).
* CONV and CAST
*    lcl_new_features_basic=>conv_and_cast( ).
* Value, NEW
*    lcl_new_features_basic=>value_and_new_commands( ).
* Ref command
*    lcl_new_features_basic=>ref_command( ).
* Internal table
    lcl_new_features_basic=>internal_table( ).
  ENDMETHOD.
  METHOD inline_declarations_and_itab.

* # String template
*    cl_demo_output=>display( |5 + 5 = { 5 + 5 } | ).

    TYPES: BEGIN OF ty_fruit,
             name TYPE string,
             color TYPE string,
           END OF ty_fruit.
    TYPES ty_t_fruit TYPE STANDARD TABLE OF ty_fruit
                     WITH NON-UNIQUE KEY name.

    DATA(lt_fruit) = VALUE ty_t_fruit(
                           ( name = 'Banana' color = 'Yellow' )
                           ( name = 'Apple' color = 'Red' )
                           ( name = 'Apple' color = 'Green' )
                           ( name = 'Orange' color = 'Orange' )
                           ( name = 'Guava' color = 'Green' )
                           ( name = 'Strawberry' color = 'Red' )
                           ( name = 'Grape' color = 'Green' )
                           ( name = 'Grape' color = 'Purple' )
                           ).

* # Print table
*    cl_demo_output=>display( data = lt_fruit ).

* # Is exists
*    DATA: lv_msg TYPE string.
*    DATA(lv_name) = 'Orange555'.
*    IF line_exists( lt_fruit[ name = lv_name ] ).
*      lv_msg = |Yes, we have { lv_name } | &
*               | at index { line_index( lt_fruit[ name = lv_name ] ) }|.
*    ELSE.
*      lv_msg = |Oops! { lv_name } is not found|.
*    ENDIF.
*    cl_demo_output=>display( data = lv_msg ).

* # Line index
*   DATA(lv_index) = line_index( lt_fruit[ name = 'Grape' ] ).
*   IF lv_index IS NOT INITIAL.
*     cl_demo_output=>display( data = lv_index ).
*   ELSE.
*     cl_demo_output=>display( data = 'not found' ).
*   ENDIF.

* # Read line by key name
*    DATA(ls_fruit) = lt_fruit[ name = 'Banana' ].
*    cl_demo_output=>display( data = ls_fruit ).

* # Read line by index
*    DATA(ls_fruit) = lt_fruit[ 2 ].
*    cl_demo_output=>display( data = ls_fruit ).

* # Advanced Internal table
    TYPES:
      BEGIN OF ty_price,
        unit TYPE string,
        price TYPE i,
      END OF ty_price.
    TYPES ty_t_price TYPE STANDARD TABLE OF ty_price WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_vegetables,
        name TYPE string,
        price TYPE ty_t_price,
      END OF ty_vegetables.
    TYPES ty_t_vegetables TYPE STANDARD TABLE OF ty_vegetables
                          WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_shelf,
        zone TYPE string,
        vegets TYPE ty_t_vegetables,
      END OF ty_shelf.
    TYPES ty_t_shelf TYPE STANDARD TABLE OF ty_shelf
                     WITH NON-UNIQUE KEY zone.

    DATA(lt_shelf) =
         VALUE ty_t_shelf(
           ( zone = 'Asia'
             vegets = VALUE ty_t_vegetables(
                        ( name = 'Carrot'
                          price = VALUE #(
                                    ( unit = 'piece' price = 5 )
                                    ( unit = 'kg' price = 50 )
                                  )
                        )
                        ( name = 'Sweet Potato'
                          price = VALUE #(
                                    ( unit = 'piece' price = 7 )
                                    ( unit = 'kg' price = 55 )
                                    ( unit = 'box' price = 250 )
                                  )
                        )
                        ( name = 'Pumpkin'
                          price = VALUE #(
                                    ( unit = 'piece' price = 15 )
                                    ( unit = 'kg' price = 100 )
                                  )
                        )
                        ( name = 'broccoli'
                          price = VALUE ty_t_price(
                            ( unit = 'piece' price = 15 )
                            ( unit = 'pack' price = 40 )
                          )
                        )
             )
           )
           ( zone = 'Europe'
             vegets = VALUE ty_t_vegetables(
                        ( name = 'Carrot'
                          price = VALUE #(
                                    ( unit = 'piece' price = 7 )
                                    ( unit = 'kg' price = 66 )
                                  )
                        )
                        ( name = 'Pumpkin'
                          price = VALUE #(
                                    ( unit = 'piece' price = 22 )
                                    ( unit = 'kg' price = 120 )
                                  )
                        )
             )
           )
         ).

*    cl_demo_output=>display( data = lt_shelf[ 1 ]-vegets[ 2 ]-price[ 3 ] ).
*    cl_demo_output=>display( data = lt_shelf[ 1 ]-vegets[ 2 ] ).

* # COND ( IF-ELSE + Action )
  DATA(lv_time) = COND string(
    WHEN sy-timlo BETWEEN '060000' AND '180000' THEN
      |Day time|
    WHEN sy-timlo BETWEEN '060000' AND '180000' THEN
      |Night time|
*    ELSE
*      THROW
  ).

*  cl_demo_output=>display( lv_time ).

* Switch (CASE + Action )
  DATA(lo_goods_movement) = NEW lcl_post_goods_mvt( ).

  DATA(lv_action) = 'GI'.

*  lo_goods_movement->post(
*    SWITCH bwart( lv_action
*      WHEN 'GR' THEN '101'
*      WHEN 'GI' THEN '103'
*    )
*  ).

    TYPES:
      BEGIN OF ty_folder,
        name TYPE c LENGTH 20,
      END OF ty_folder.
    TYPES ty_t_folder TYPE STANDARD TABLE OF  ty_folder WITH EMPTY KEY.

    DATA: lt_result TYPE STANDARD TABLE OF string WITH EMPTY KEY.

* Internal table of folder
    DATA(lt_folder) = VALUE ty_t_folder(
                            ( name = 'Temp' )
                            ( name = 'Koi' )
                            ( name = 'Rabbit' )
                            ).

* # CONV

    LOOP AT lt_folder INTO DATA(ls_folder).
      DATA(lv_result) = SWITCH #( cl_gui_frontend_services=>directory_exist(
                                     |D:\\{ ls_folder-name }| )
                                  WHEN abap_true THEN |directory { ls_folder-name } exists|
                                  WHEN abap_false THEN |there is no directory { ls_folder-name }|
                                  ).
      APPEND lv_result TO lt_result.
    ENDLOOP.

*    cl_demo_output=>display( lt_result ).

  ENDMETHOD.
  METHOD conv_and_cast.

  DATA: lt_result TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  DATA: lv_bool TYPE string.

  IF ( 1 / 3 ) > 0.
    lv_bool = 'True'.
  ELSE.
    lv_bool = 'False'.
  ENDIF.

  APPEND |1 / 3 > 0 = { lv_bool }| TO lt_result.
  CLEAR lv_bool.

  IF CONV f( 1 / 3 ) > 0.
    lv_bool = 'True'.
  ELSE.
    lv_bool = 'False'.
  ENDIF.

  APPEND |f( 1 / 3 ) > 0 = { lv_bool }| TO lt_result.
  CLEAR lv_bool.

*  cl_demo_output=>display( lt_result ).


* # CAST
    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr.

*    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( 'T100' ).
*    IF sy-subrc = 0.
*      cl_demo_output=>display( lo_structdescr->components ).
*    ENDIF.

    cl_demo_output=>display( CAST cl_abap_structdescr(
                             cl_abap_typedescr=>describe_by_name( 'T100' ) )->components
                            ).
  ENDMETHOD.
  METHOD value_and_new_commands.

    DATA: lo_round TYPE REF TO zcl_round.

    lo_round = NEW #( 5 ).
    lo_round->cal_area( ).
*    lo_round->print_shape_info( ).

    DATA(lo_square) = NEW zcl_square( 3 ).
    lo_square->cal_area( ).
*    lo_square->print_shape_info( ).


    DATA: lo_box1 TYPE REF TO lcl_box,
          lo_box2 TYPE REF TO lcl_box,
          lo_box3 TYPE REF TO lcl_box.
    DATA: lt_result TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA: lv_bool TYPE string.

    DATA: ls_dimension TYPE lcl_box=>ty_dimension.

    lo_box1 = NEW lcl_box( VALUE lcl_box=>ty_dimension( w = 2 l = 2 h = 2 ) ).

    ls_dimension-w = 2.
    ls_dimension-l = 2.
    ls_dimension-h = 2.

    lo_box2 = NEW #( ls_dimension ).

    lo_box3 = lo_box1.

    APPEND |box1's size = { lo_box1->get_boxsize( ) }| TO lt_result.
    APPEND |box2's size = { lo_box2->get_boxsize( ) }| TO lt_result.
    APPEND |box2's size = { lo_box2->get_boxsize( ) }| TO lt_result.
    APPEND || TO lt_result.
    lv_bool = COND #( WHEN lo_box1 = lo_box2 THEN 'True'
                      ELSE 'False' ).
    APPEND |box1 = box2 is { lv_bool }| TO lt_result.
    lv_bool = COND #( WHEN lo_box1 = lo_box3 THEN 'True'
                      ELSE 'False' ).
    APPEND |box1 = box3 is { lv_bool }| TO lt_result.

    cl_demo_output=>display( lt_result ).

*    DATA(lo_hawk) = NEW lcl_bird( ).
*
*    lo_hawk->speak( ).
  ENDMETHOD.
  METHOD ref_command.
    DATA: lv_carrid TYPE scarr-carrid,
          lv_name TYPE scarr-carrname.

    DATA(key) = 'TG'.

    DATA(sql) = NEW cl_sql_statement( ).

    sql->set_param( REF #( sy-mandt ) ).
    sql->set_param( REF #( key ) ).

    TRY .
      DATA(result) = sql->execute_query(
        'SELECT SINGLE CARRNAME ' &&
        'FROM SCARR ' &&
        'WHERE MANDT = ? AND CARRID = ?' ).

      result->set_param( REF #( lv_name ) ).
      result->next( ).
      cl_demo_output=>display( lv_name ).
    CATCH cx_sql_exception INTO DATA(lx_sql_exception).

    ENDTRY.


  ENDMETHOD.
  METHOD internal_table.
    TYPES ty_t_scarr TYPE STANDARD TABLE OF scarr WITH DEFAULT KEY.

    DATA(lt_scarr) = VALUE ty_t_scarr( ( mandt = sy-mandt
                                         carrid = 'AA'
                                         carrname = 'American Airlines'
                                         currcode = 'USD'
                                         url = space )
                                       ( mandt = sy-mandt
                                         carrid = 'AF'
                                         carrname = 'Air France '
                                         currcode = 'EUR'
                                         url = space )
                                       ( mandt = sy-mandt
                                         carrid = 'TG'
                                         carrname = 'Thai Airways'
                                         currcode = 'THB'
                                         url = space )
                                       ( mandt = sy-mandt
                                         carrid = 'JP'
                                         carrname = 'Japan Airlines '
                                         currcode = 'JPY'
                                         url = space )
                                      ).

    DATA ls_scarr TYPE scarr.

    ls_scarr = lt_scarr[ 2 ].
*    cl_demo_output=>display( ls_scarr ).
    ls_scarr = lt_scarr[ carrid = 'TG' ].
*    cl_demo_output=>display( ls_scarr ).

*    cl_demo_output=>display(
*                             lt_scarr[ lines( lt_scarr ) ]
*                            ).

    TRY .
      ls_scarr = lt_scarr[ carrid = 'TG' ].

      cl_demo_output=>display( ls_scarr ).
    CATCH cx_sy_itab_line_not_found.

    ENDTRY.


*    ls_scarr = COND #( WHEN line_exists( lt_scarr[ carrid = 'TG1' ] )
*                            THEN lt_scarr[ carrid = 'TG1' ]
*                       ).
*
*    cl_demo_output=>display( ls_scarr ).
  ENDMETHOD.
ENDCLASS.
