*&---------------------------------------------------------------------*
*& Report zdemo_abap_new_features
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_abap_new_features.

CLASS lcl_abap_new_features_740 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS string_template_and_format.
    CLASS-METHODS inline_declarations.
    CLASS-METHODS operators_new_value_and_ref.
    CLASS-METHODS operators_conv_and_cast.
    CLASS-METHODS operators_cond_and_switch.
    CLASS-METHODS internal_table.
    CLASS-METHODS abap_object.
ENDCLASS.

CLASS lcl_abap_new_features_740_sp5 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS internal_table.
    CLASS-METHODS corresponding_operator.
    CLASS-METHODS open_sql.
    CLASS-METHODS meshes.
ENDCLASS.

CLASS lcl_abap_new_features_740_sp8 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS expressions.
    CLASS-METHODS internal_table.
    CLASS-METHODS database_access.
ENDCLASS.

CLASS lcl_abap_new_features_750 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS fundamentals.
    CLASS-METHODS assignments_and_expressions.
    CLASS-METHODS messages_and_exceptions.
    CLASS-METHODS open_sql.
    CLASS-METHODS abap_cds.
    CLASS-METHODS external_interfaces.

ENDCLASS.

CLASS lcl_abap_new_features_751 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
    CLASS-METHODS data_types.
    CLASS-METHODS assignments.
    CLASS-METHODS abap_cds_and_open_sql.
    CLASS-METHODS further_news.
ENDCLASS.

CLASS lcl_coffee_shop DEFINITION.
  PUBLIC SECTION.
    TYPES ty_price TYPE p LENGTH 15 DECIMALS 2.
    TYPES:
      BEGIN OF ty_sub_menu,
        name  TYPE string,
        price TYPE ty_price,
      END OF ty_sub_menu.
    TYPES ty_t_sub_menu TYPE STANDARD TABLE OF ty_sub_menu WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_menu,
        name     TYPE string,
        special  TYPE abap_bool,
        price    TYPE ty_price,
        sub_menu TYPE ty_t_sub_menu,
      END OF ty_menu.
    TYPES: ty_t_menu TYPE STANDARD TABLE OF ty_menu
                     WITH NON-UNIQUE KEY name.

    CONSTANTS: special_menu TYPE c VALUE 'S'.

    METHODS constructor.
    METHODS is_menu_exist IMPORTING i_name       TYPE string
                          EXPORTING et_sub_menu  TYPE ty_t_sub_menu
                          RETURNING VALUE(value) TYPE char1.

  PRIVATE SECTION.
    DATA: lt_menu TYPE ty_t_menu.
ENDCLASS.

CLASS shirt DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ENUM tsize,
        size_s,
        size_m,
        size_l,
        size_xl,
      END OF ENUM tsize.

    TYPES ty_price TYPE p LENGTH 15 DECIMALS 2.

    METHODS constructor IMPORTING size TYPE tsize.
    METHODS get_price RETURNING VALUE(value) TYPE ty_price.

  PRIVATE SECTION.
    DATA: size  TYPE tsize,
          price TYPE ty_price.

    METHODS set_prize_tag.
ENDCLASS.

CLASS shirt IMPLEMENTATION.
  METHOD constructor.
    me->size = size.

* Set price tag
    me->set_prize_tag( ).
  ENDMETHOD.

  METHOD set_prize_tag.
    me->price = SWITCH ty_price( me->size
                             WHEN size_s THEN 150
                             WHEN size_m THEN 200
                             WHEN size_l THEN 250
                             WHEN size_xl THEN 300 ).
  ENDMETHOD.
  METHOD get_price.
    value = price.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_coffee_shop IMPLEMENTATION.

  METHOD constructor.

* Menu
    lt_menu = VALUE #( ( name = 'coffee'
                         sub_menu = VALUE #( ( name = 'Espresso'
                                               price = 50
                                             )
                                             ( name = 'Latte'
                                               price = 60
                                             )
                                             ( name = 'Mocha'
                                               price = 55
                                             )
                                             ( name = 'Cappuccino'
                                               price = 50
                                             )
                                             ( name = 'Caramel Macchiato'
                                               price = 50
                                             )
                                           )
                       )
                       ( name = 'chocolatte'
                         sub_menu = VALUE #( ( name = 'Chocolatte Classique'
                                               price = 45
                                             )
                                             ( name = 'Dark Choco'
                                               price = 65
                                             )
                                             ( name = 'Premium Choco'
                                               price = 75
                                             )
                                           )
                       )
                       ( name = 'tea'
                         sub_menu = VALUE #( ( name = 'Black Tea'
                                               price = 40
                                             )
                                             ( name = 'Thai Tea'
                                               price = 45
                                             )
                                             ( name = 'Match'
                                               price = 70
                                             )
                                           )
                       )
                       ( name = 'O-Lieng'
                         special = abap_true
                         price = 35
                       )
                       ( name = 'Honey Lemon'
                         special = abap_true
                         price = 45
                       )

                     ).
  ENDMETHOD.

  METHOD is_menu_exist.
    TRY.

        IF et_sub_menu IS SUPPLIED.
* Check special menu
          CASE lt_menu[ name = i_name ]-special.
            WHEN abap_true.
* Return flag
              value = special_menu. "Special

              et_sub_menu = VALUE ty_t_sub_menu( ( name = i_name
                                                   price = lt_menu[ name = i_name ]-price
                                               )
                                             ).
            WHEN space.
* Return flag
              value = abap_true.

* Sub menu
              et_sub_menu = lt_menu[ name = i_name ]-sub_menu.
          ENDCASE.
        ELSE.
          IF line_exists( lt_menu[ name = i_name ] ).
* Return flag
            value = abap_true.
          ENDIF.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        value = space.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_msg DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_t100_message.
    ALIASES: get_text FOR if_message~get_text,
             get_longtext FOR if_message~get_longtext.
    METHODS constructor IMPORTING id TYPE symsgid
                                  no TYPE symsgno
                                  v1 TYPE string OPTIONAL
                                  v2 TYPE string OPTIONAL
                                  v3 TYPE string OPTIONAL
                                  v4 TYPE string OPTIONAL.
    DATA: attr1 TYPE c LENGTH 50,
          attr2 TYPE c LENGTH 50,
          attr3 TYPE c LENGTH 50,
          attr4 TYPE c LENGTH 50.
ENDCLASS.

CLASS lcl_msg IMPLEMENTATION.
  METHOD constructor.
    if_t100_message~t100key-msgid = id.
    if_t100_message~t100key-msgno = no.
    if_t100_message~t100key-attr1 = 'ATTR1'.
    if_t100_message~t100key-attr2 = 'ATTR2'.
    if_t100_message~t100key-attr3 = 'ATTR3'.
    if_t100_message~t100key-attr4 = 'ATTR4'.
    attr1 = v1.
    attr2 = v2.
    attr3 = v3.
    attr4 = v4.
  ENDMETHOD.
  METHOD if_message~get_text.
    result = cl_message_helper=>get_text_for_message( me ).
  ENDMETHOD.
  METHOD if_message~get_longtext.
    result = cl_message_helper=>get_longtext_for_message( me ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_using_system_interface_msg DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_using_system_interface_msg IMPLEMENTATION.

  METHOD main.

    DATA(lo_msg1) = NEW lcl_msg( id = CONV #( 'SABAPDEMOS' )
                                 no = CONV #( '888' )
                                 v1 = |This is a book|
                                 v2 = space ).
    MESSAGE lo_msg1 TYPE 'I'.

    DATA(lo_msg2) = NEW lcl_msg( id = CONV #( 'SABAPDEMOS' )
                                 no = CONV #( '007' ) ).
    MESSAGE lo_msg2 TYPE 'W' DISPLAY LIKE 'E'.
*    MESSAGE lo_msg2 TYPE 'E'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  " DATA(lv_number) = 3.
  " DATA(lv_test) = `this is a test { lv_number }`.
* Call Main Method
*  lcl_abap_new_features_740=>main( ).
*  lcl_abap_new_features_740_sp5=>main( ).
*  lcl_abap_new_features_740_sp8=>main( ).
*  lcl_abap_new_features_750=>main( ).
  lcl_abap_new_features_751=>main( ).

CLASS lcl_abap_new_features_740 IMPLEMENTATION.

  METHOD main.
* # String template
*    lcl_abap_new_features_740=>string_template_and_format( ).

* # In-line Declarations
*    lcl_abap_new_features_740=>inline_declarations( ).

* # Constructor Expressions
* ## Operator NEW, VALUE and REF
*    lcl_abap_new_features_740=>operators_new_value_and_ref( ).

* ## Operators CONV and CAST
*    lcl_abap_new_features_740=>operators_conv_and_cast( ).

* ## Operators COND and SWITCH
*    lcl_abap_new_features_740=>operators_cond_and_switch( ).

* # Internal table
*    lcl_abap_new_features_740=>internal_table( ).

* # ABAP Object
    lcl_abap_new_features_740=>abap_object( ).

  ENDMETHOD.

  METHOD inline_declarations.

    DATA(lo_output) = cl_demo_output=>new( ).

* Inline declarations
    lo_output->next_section( 'Inline declarations' ).
* Get data from table SCARR
    SELECT *
    FROM scarr INTO TABLE @DATA(lt_scarr).

    SELECT carrid, carrname
    FROM scarr INTO TABLE @DATA(lt_scarr_x).

* Table contents
    lo_output->write( |Table contents| ).
    lo_output->write( lt_scarr ).
    lo_output->line( ).

* Airline list
    lo_output->write( 'list of Airline' ).
    LOOP AT lt_scarr INTO DATA(ls_scarr).
      lo_output->write( ls_scarr-carrname ).
    ENDLOOP.
    lo_output->line( ).

** Show table SCARR in ALV
*    TRY.
*      cl_salv_table=>factory(
*        IMPORTING
*          r_salv_table   = DATA(lo_salv)
*        CHANGING
*          t_table        = lt_scarr ).
*
*        IF lo_salv IS NOT INITIAL .
** Set function
*          lo_salv->get_functions( )->set_all( abap_true ).
*
** Display table
*          lo_salv->display( ).
*        ENDIF.
*      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
*        lo_output->write( lx_salv_msg->get_message( ) ).
*    ENDTRY.

* Access table contents
    lo_output->next_section( 'Access table contents' ).
* Read line by index
    lo_output->write( 'Read line index 1' ).
    DATA(ls_scarr_x) = lt_scarr_x[ 1 ].
    IF ls_scarr_x IS NOT INITIAL.
      lo_output->write( ls_scarr_x-carrname ).
    ENDIF.
    lo_output->write( ls_scarr_x ).
    lo_output->line( ).

* Read line by key
    lo_output->write( 'Read line by key CARRID = TG' ).
    CLEAR ls_scarr_x.
    ls_scarr_x = lt_scarr_x[ carrid = 'TG' ].
    lo_output->write( ls_scarr_x ).
    lo_output->line( ).

    lo_output->display( ).

  ENDMETHOD.

  METHOD operators_cond_and_switch.

    TYPES:
      BEGIN OF ty_folder,
        name TYPE c LENGTH 20,
      END OF ty_folder.
    TYPES ty_t_folder TYPE STANDARD TABLE OF  ty_folder WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_invoice,
        inv_no   TYPE vbeln,
        desc     TYPE string,
        price    TYPE p LENGTH 15 DECIMALS 2,
        currency TYPE c LENGTH 5,
      END OF ty_invoice.
    TYPES ty_t_invoice TYPE STANDARD TABLE OF ty_invoice
                       WITH DEFAULT KEY.

    DATA(lo_output) = cl_demo_output=>new( ).

* # COND
    lo_output->next_section( 'COND Operator').
    DATA(lv_time) = COND string(
      WHEN sy-timlo BETWEEN '060000' AND '180000' THEN
                    |Day time|
      WHEN sy-timlo BETWEEN '060000' AND '180000' THEN
                    |Night time| ).

    lo_output->write( lv_time ).

* Internal table of folder
    DATA(lt_folder) = VALUE ty_t_folder(
                            ( name = 'Temp' )
                            ( name = 'Koi' )
                            ( name = 'Rabbit' )
                            ).

    LOOP AT lt_folder INTO DATA(ls_folder).
      DATA(lv_result) = SWITCH #( cl_gui_frontend_services=>directory_exist(
                                     |D:\\{ ls_folder-name }| )
                                  WHEN abap_true THEN |directory { ls_folder-name } exists|
                                  WHEN abap_false THEN |there is no directory { ls_folder-name }|
                                  ).
      lo_output->write( lv_result ).
    ENDLOOP.
    lo_output->line( ).

* # SWITCH
    lo_output->next_section( 'SWITCH Operator').
    DATA(lv_i) = 1.

    DATA(lt_inv) = VALUE ty_t_invoice( ( inv_no = |1|
                                         desc = |IPad|
                                         price = 5000
                                         currency = 'USD'
                                       )
                                       ( inv_no = |2|
                                         desc = |IPhone|
                                         price = 3500
                                         currency = 'THB'
                                       )
                                       ( inv_no = |3|
                                         desc = |IPod|
                                         price = 1500
                                         currency = 'EUR'
                                       )
                                       ( inv_no = |4|
                                         desc = |Macbook|
                                         price = 455000000
                                         currency = 'JPY'
                                       )
                                     ).

    LOOP AT lt_inv INTO DATA(ls_inv).

      lo_output->write( |Invoice no. { ls_inv-inv_no }| &
                        | Desc: { ls_inv-desc } | &
                        | Price: { ls_inv-price CURRENCY = ls_inv-currency } | &
                        | Currency: { SWITCH string( ls_inv-currency
                                        WHEN 'USD' THEN 'Dollar'
                                        WHEN 'THB' THEN 'Bath'
                                        WHEN 'EUR' THEN 'Euro'
                                        WHEN 'JPY' THEN 'Yen' ) }| ).
    ENDLOOP.
    lo_output->line( ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD operators_conv_and_cast.
    DATA: lv_bool TYPE string.

    DATA(lo_output) = cl_demo_output=>new( ).

* CONV command
    lo_output->next_section( 'CONV operator').
    IF ( 1 / 3 ) > 0.
      lv_bool = 'True'.
    ELSE.
      lv_bool = 'False'.
    ENDIF.
    lo_output->write( |(1 / 3 ) > 0 = { lv_bool }| ).
    CLEAR lv_bool.

    IF CONV f( 1 / 3 ) > 0.
      lv_bool = 'True'.
    ELSE.
      lv_bool = 'False'.
    ENDIF.
    lo_output->write( |CONV f(1 / 3 ) > 0 = { lv_bool }| ).
    CLEAR lv_bool.
    lo_output->line( ).

* CAST command
    lo_output->next_section( 'CAST operator').

* With helper
    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr.

    lo_output->write( 'With Helper object').
    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( 'T100' ).
    IF sy-subrc = 0.
      lo_output->write( lo_structdescr->components ).
    ENDIF.

    lo_output->write( 'Without helper object').
    lo_output->write(
            CAST cl_abap_structdescr(
                 cl_abap_typedescr=>describe_by_name( 'T100' ) )->components
                                    ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD operators_new_value_and_ref.

    TYPES:
      BEGIN OF ty_animals,
        name  TYPE string,
        feet  TYPE i,
        sound TYPE string,
      END OF ty_animals.
    TYPES ty_t_animals TYPE STANDARD TABLE OF ty_animals
                       WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_kind_of_animals,
        kind    TYPE string,
        animals TYPE ty_t_animals,
      END OF ty_kind_of_animals.
    TYPES ty_t_kind_of_animals TYPE STANDARD TABLE OF
          ty_kind_of_animals WITH NON-UNIQUE KEY kind.


    DATA(lo_output) = cl_demo_output=>new( ).

* VALUE operator
    lo_output->next_section( 'VALUE Operator' ).

* Value for a structure
    DATA(ls_animals) = VALUE ty_animals( name = 'Bear'
                                         feet = 4
                                         sound = 'Roar'
                                       ).
    lo_output->write( |Value for Structure| ).
    lo_output->write( ls_animals ).
    lo_output->line( ).

* Value for an internal table
    DATA(lt_animals) = VALUE ty_t_animals(
                                           ( ls_animals )
                                           ( name = 'Tiger'
                                             feet = 4
                                             sound = 'Growl'
                                           )
                                           ( name = 'Duck'
                                             feet = 2
                                             sound = 'Quack'
                                           )
                                         ).
    lo_output->write( |Value for Internal table| ).
    lo_output->write( lt_animals ).
    lo_output->line( ).

* Value for a complex internal table
    DATA(lt_kind_of_animals) = VALUE ty_t_kind_of_animals(
         ( kind = 'Farm'
           animals = VALUE #( ( name = 'Hen'
                                feet = 2
                                sound = 'cluck'
                              )
                              ( name = 'Rooster'
                                feet = 2
                                sound = 'cock-a-doodle-doo'
                              )
                              ( name = 'Pig'
                                feet = 4
                                sound = 'oink'
                              )
                              ( name = 'Cow'
                                feet = 4
                                sound = 'Moo'
                              )
                              ( name = 'Pig'
                                feet = 4
                                sound = 'oink'
                              )
                            )
         )
         ( kind = 'Pet'
           animals = VALUE #( ( name = 'Dog'
                                 feet = 4
                                 sound = 'Bark'
                               )
                               ( name = 'Cat'
                                 feet = 4
                                 sound = 'meow'
                               )
                             )
         )
         ( kind = 'Wild'
           animals = VALUE ty_t_animals( ( name = 'Elephant'
                                           feet = 4
                                           sound = 'trumpet'
                                         )
                                         ( name = 'Zebra'
                                           sound = 'bray'
                                         )
                                       )
         )
       ).

    lo_output->write( |Value for complex Internal table| ).
    lo_output->write( |Total lines of LT_KIND_OF_ANIMALS is { lines( lt_kind_of_animals ) } | ).

    LOOP AT lt_kind_of_animals INTO DATA(ls_kind_of_animals).
      lo_output->write( |line index { sy-tabix } is { ls_kind_of_animals-kind } | ).
      lo_output->write( ls_kind_of_animals-animals ).
    ENDLOOP.
    lo_output->line( ).

* NEW operator
    DATA: lo_round1 TYPE REF TO zcl_round.

    lo_output->next_section( 'NEW operator' ).

    lo_round1 = NEW #( 3 ).
    lo_round1->cal_area( ).
    lo_output->write( |Object Round1 info: { lo_round1->get_shape_info( ) }| ).

    DATA(lo_round2) = NEW zcl_round( 4 ).
    lo_round2->cal_area( ).
    lo_output->write( |Object Round2 info: { lo_round2->get_shape_info( ) }| ).

    lo_output->line( ).


    lo_output->display( ).

  ENDMETHOD.

  METHOD string_template_and_format.

    DATA(lo_output) = cl_demo_output=>new( ).

* String template
    lo_output->next_section( 'String Template' ).

    lo_output->write( |5 + 5 = { 5 + 5 } | ).
    lo_output->line( ).

* Text alignment
    lo_output->write( 'Text alignment' ).
    DATA(lv_text) = 'This is SAP ABAP ver. 7.40 new features'.
    lo_output->write( |Left: { lv_text WIDTH = 100 ALIGN = LEFT }| ).
    lo_output->write( |Center: { lv_text WIDTH = 100 ALIGN = CENTER }| ).
    lo_output->write( |Right: { lv_text WIDTH = 100 ALIGN = RIGHT }| ).
    lo_output->line( ).

* CASE
    lo_output->write( 'Text case' ).
    DATA(lv_case) = 'This is A booK'.
    lo_output->write( |Raw: { lv_case CASE = (cl_abap_format=>c_raw) }| ).
    lo_output->write( |Upper: { lv_case CASE = (cl_abap_format=>c_upper) }| ).
    lo_output->write( |Lower: { lv_case CASE = (cl_abap_format=>c_lower) }| ).
    lo_output->line( ).

* Date & Time format
    lo_output->write( 'Date and Time format').

    DATA(lv_date_time) = |Current date is { sy-datum DATE = USER }| &
                         | and current time is { sy-uzeit TIME = USER }|.

    lo_output->write( lv_date_time ).
    lo_output->line( ).

* ALPHA
    DATA lv_vbeln TYPE vbeln VALUE '0000012345'.

    lo_output->write( |ALPHA In: Billing number { lv_vbeln ALPHA = IN }| ).
    lo_output->write( |ALPHA Out: Billing number { lv_vbeln ALPHA = OUT }| ).
    lo_output->line( ).


* Concatenate
    lo_output->write( 'Concatenate' ).
    DATA(lv_concate) = |I am a robot| && | Current client = { sy-mandt }| && ' Banana'.
    lo_output->write( lv_concate ).
    lo_output->write( |Line 1 +| &
                      |Line 2 +| &
                      |Line 3| ).
    lo_output->line( ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD internal_table.
    TYPES:
      BEGIN OF ty_fruit_color,
        color TYPE string,
      END OF ty_fruit_color.
    TYPES ty_t_fruit_color TYPE STANDARD TABLE OF
                           ty_fruit_color WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_fruit,
        name      TYPE string,
        skin_type TYPE c LENGTH 1,
        colors    TYPE ty_t_fruit_color,
      END OF ty_fruit.

    DATA(lo_output) = cl_demo_output=>new( ).

    DATA lt_fruit TYPE STANDARD TABLE OF ty_fruit.

    DATA: lt_name  TYPE STANDARD TABLE OF string,
          lt_index TYPE STANDARD TABLE OF i.
    DATA: lv_name  TYPE string,
          lv_index TYPE i.

* Initial value for Internal table
    lo_output->next_section( 'Internal table' ).

    lt_fruit = VALUE #( ( name = 'Banana'
                          skin_type = 'S'
                          colors = VALUE #( ( color = 'Green' )
                                            ( color = 'Yellow' )
                                          )
                        )
                        ( name = 'Mango'
                          skin_type = 'S'
                          colors = VALUE #( ( color = 'Green' )
                                            ( color = 'Yellow' )
                                          )
                        )
                        ( name = 'Coconut'
                          skin_type = 'H'
                          colors = VALUE #( ( color = 'Green' )
                                            ( color = 'Brown' )
                                          )
                        )
                        ( name = 'Grape'
                          skin_type = 'S'
                          colors = VALUE #( ( color = 'Green' )
                                            ( color = 'Purple' )
                                          )
                        )
                        ( name = 'Orange'
                          skin_type = 'M'
                          colors = VALUE #( ( color = 'Green' )
                                            ( color = 'Orange' )
                                          )
                        )
                        ( name = 'Strawberry'
                          skin_type = 'S'
                          colors = VALUE #( ( color = 'White' )
                                            ( color = 'Red' )
                                          )
                        )
                      ).


* Function Line existence and Line index
    lo_output->next_section( 'Line existence and Line index built-in functions' ).

* Name to be read
    lt_name = VALUE #( ( CONV string( 'Mango' ) )
                       ( CONV string( 'Apple' ) )
                       ( CONV string( 'Grape' ) )
                       ( CONV string( 'Cherry' ) )
                       ( CONV string( 'Strawberry' ) )
                     ).

    LOOP AT lt_name INTO lv_name.
      lo_output->write( |Is { lv_name } in the table? : {
                        COND string( WHEN  line_exists( lt_fruit[ name = lv_name ] )
                                         THEN |Yes it is at line { line_index( lt_fruit[ name = lv_name ] ) }|
                                   ELSE 'No it is not' )
                                               }|
                    ).
    ENDLOOP.
    lo_output->line( ).

* Access table by index
    lo_output->next_section( 'Access table contents' ).

    CLEAR lt_index.

    lt_index = VALUE #( ( 3 )
                        ( 11 )
                        ( 5 )
                        ( 6 )
                        ( 9 )
                      ).

    lo_output->write( 'Access table by index' ).
    LOOP AT lt_index INTO lv_index.
      TRY.
          lo_output->write( |Color of index { lv_index } ({ lt_fruit[ lv_index ]-name }) are| ).
          lo_output->write( lt_fruit[ lv_index ]-colors ).
        CATCH cx_sy_itab_line_not_found.
          lo_output->write( |Index { lv_index } does not found| ).
      ENDTRY.
    ENDLOOP.

    lo_output->line( ).

* Access table by key

    lo_output->write( 'Access table by key' ).

* Add names to be read
    APPEND 'Coconut' TO lt_name.
    INSERT 'Banana' INTO lt_name INDEX 1.
    APPEND 'Orange' TO lt_name.

    LOOP AT lt_name INTO lv_name.
      TRY.

          DATA(ls_fruit) = lt_fruit[ name = lv_name ].

          lo_output->write( |{ ls_fruit-name } has | &
                            |{
                             SWITCH string( ls_fruit-skin_type
                                            WHEN 'S' THEN 'soft'
                                            WHEN 'M' THEN 'medium'
                                            WHEN 'H' THEN 'hard'
                                          )
                            } skin|
                          ).
        CATCH cx_sy_itab_line_not_found.
          lo_output->write( |{ lv_name } is not found in the list| ).
      ENDTRY.
    ENDLOOP.
    lo_output->line( ).

* Modify table contents
    lo_output->next_section( 'Modify table contents' ).

* Interested index
    lv_index = 1.

    lo_output->write( |Data in record { lv_index } (Before)| ).
    lo_output->write( |{ lt_fruit[ lv_index ]-name } + | &
                      |{ lt_fruit[ lv_index ]-skin_type } + | &
                      |{ lines( lt_fruit[ lv_index ]-colors ) }| ).

    lt_fruit[ lv_index ]-skin_type = 'M'.
    ASSIGN lt_fruit[ lv_index ]-colors TO FIELD-SYMBOL(<colors_tab>).
    IF sy-subrc = 0.
      DATA(ls_fc) = VALUE ty_fruit_color( color = 'Pink' ).
      APPEND ls_fc TO <colors_tab>.
    ENDIF.

    lo_output->write( |Data in record { lv_index } (After)| ).
    lo_output->write( |{ lt_fruit[ lv_index ]-name } + | &
                      |{ lt_fruit[ lv_index ]-skin_type } + | &
                      |{ lines( lt_fruit[ lv_index ]-colors ) }| ).

    lo_output->line( ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD abap_object.
    TYPES ty_t_order TYPE STANDARD TABLE OF char20 WITH EMPTY KEY.

    DATA(lo_coffee_shop) = NEW lcl_coffee_shop( ).
    DATA lt_sub_menu TYPE lcl_coffee_shop=>ty_t_sub_menu.

    DATA(lo_output) = cl_demo_output=>new( ).

* ABAP Object
    lo_output->next_section( 'ABAP Object ' ).
    lo_output->write( 'Functinal method now support Export and Changing' ).

    DATA(lt_order) = VALUE ty_t_order( ( 'coffee' )
                                       ( 'orange juice')
                                       ( 'tea' )
                                       ( 'Honey Lemon')
                                     ).

    LOOP AT lt_order ASSIGNING FIELD-SYMBOL(<order>).
      CASE lo_coffee_shop->is_menu_exist(
                           EXPORTING i_name = CONV string( <order> )
                           IMPORTING et_sub_menu = lt_sub_menu ).
        WHEN space.
          lo_output->write( |Menu { <order> } does not exist| ).
        WHEN lcl_coffee_shop=>special_menu.
          lo_output->write( |Enjoy { <order> } just { lt_sub_menu[ 1 ]-price } bath ony| ).
        WHEN abap_true.
          lo_output->write( |We have { <order> } menu as below list, choose your feverite| ).
          lo_output->write( lt_sub_menu ).
      ENDCASE.
    ENDLOOP.

    lo_output->display( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_new_features_740_sp5 IMPLEMENTATION.
  METHOD main.

* #corresponding.
*    lcl_abap_new_features_740_sp5=>corresponding_operator( ).

* #internal_table.
*    lcl_abap_new_features_740_sp5=>internal_table( ).

* #open_sql.
*    lcl_abap_new_features_740_sp5=>open_sql( ).
  ENDMETHOD.

  METHOD corresponding_operator.
    TYPES:
      BEGIN OF ty_order,
        order_no   TYPE n LENGTH 10,
        order_date TYPE  sy-datum,
        mat_no     TYPE matnr,
        qty        TYPE i,
        unit       TYPE meins,
        remark     TYPE char40,
      END OF ty_order.
    TYPES ty_t_order TYPE STANDARD TABLE OF ty_order
                     WITH NON-UNIQUE KEY order_no.

    TYPES:
      BEGIN OF ty_text,
        long_text TYPE c LENGTH 80,
        remark    TYPE char40,
      END OF ty_text.

    TYPES:
      BEGIN OF ty_po,
        po_no   TYPE n LENGTH 10,
        po_date TYPE sy-datum,
        mat_no  TYPE matnr,
        qty     TYPE i,
        unit    TYPE meins,
        text    TYPE ty_text,
      END OF ty_po.
    TYPES ty_t_po TYPE STANDARD TABLE OF ty_po
                  WITH NON-UNIQUE KEY po_no.


    DATA: BEGIN OF ls_struc1,
            col1  TYPE c VALUE 'X',
            col2  TYPE c VALUE 'Y',
            col_a TYPE string VALUE '555',
            col_b TYPE i VALUE 1,
            col3  TYPE c VALUE 'Z',
            colx  TYPE c VALUE 'X',
          END OF ls_struc1.

    DATA: BEGIN OF ls_struc2,
            col2  TYPE c,
            col_a TYPE string,
            col_c TYPE string,
            col3  TYPE c,
            colu  TYPE c,
          END OF ls_struc2.

    DATA(lo_output) = cl_demo_output=>new( ).

    lo_output->next_section( |Move corresponding| ).
    lo_output->write( |Structure 1| ).
    lo_output->write( ls_struc1 ).
    MOVE-CORRESPONDING ls_struc1 TO ls_struc2.
    lo_output->write( |Structure 2| ).
    lo_output->write( ls_struc2 ).
    lo_output->line( ).

* Order table
    DATA(lt_order) = VALUE ty_t_order( ( order_no = 1
                                         order_date = sy-datum + 1
                                         mat_no = 'MAT1'
                                         qty = 5
                                         unit = 'EA'
                                         remark = 'Mat1' )
                                       ( order_no = 2
                                         order_date = sy-datum + 2
                                         mat_no = 'MAT2'
                                         qty = 7
                                         unit = 'BOX'
                                         remark = 'ZYX' )
                                      ).
    DATA: lt_po TYPE ty_t_po.

    MOVE-CORRESPONDING lt_order TO lt_po EXPANDING NESTED TABLES.

    lo_output->write( |Order data| ).
    lo_output->write( lt_order ).
    lo_output->line( ).

    lo_output->write( |PO data| ).
    LOOP AT lt_po INTO DATA(ls_po).
      lo_output->write( |{ ls_po-po_no } { ls_po-po_date } { ls_po-mat_no } { ls_po-qty }| &
                        |{ ls_po-unit }| &
                        |{ ls_po-text-long_text } { ls_po-text-remark }| ).

    ENDLOOP.

    lo_output->line( ).

* Corresponding
    CLEAR ls_struc2.

    lo_output->next_section( |Corresponding| ).
    lo_output->write( |Structure 1| ).
    lo_output->write( ls_struc1 ).
    ls_struc2 = CORRESPONDING #( ls_struc1 MAPPING colu = colx ).
    lo_output->write( |Structure 2| ).
    lo_output->write( ls_struc2 ).
    lo_output->line( ).

    CLEAR: lt_po.

    lt_po = CORRESPONDING #( lt_order MAPPING po_date = order_date ).

    lo_output->write( |Order data| ).
    lo_output->write( lt_order ).
    lo_output->line( ).

    lo_output->write( |PO data| ).
    LOOP AT lt_po INTO ls_po.
      lo_output->write( |{ ls_po-po_no } { ls_po-po_date } { ls_po-mat_no } { ls_po-qty }| &
                        |{ ls_po-unit }| &
                        |{ ls_po-text-long_text } { ls_po-text-remark }| ).

    ENDLOOP.


    lo_output->next_section( 'Corresponding - Lookup' ).

    TYPES:
      BEGIN OF ty_mkpf,
        mblnr TYPE c LENGTH 10,
        mjahr TYPE n LENGTH 4,
        cpudt TYPE sydatum,
      END OF ty_mkpf.
    TYPES ty_t_mkpf TYPE SORTED TABLE OF ty_mkpf
                    WITH NON-UNIQUE KEY primary_key COMPONENTS mblnr mjahr.

    TYPES:
      BEGIN OF ty_mseg,
        mblnr       TYPE c LENGTH 10,
        mjahr       TYPE n LENGTH 4,
        zeile       TYPE n LENGTH 4,
        menge       TYPE menge_d,
        meins       TYPE meins,
        create_date TYPE sydatum,
      END OF ty_mseg.
    TYPES ty_t_mseg TYPE  STANDARD TABLE OF ty_mseg
                    WITH DEFAULT KEY.

    DATA(lt_mkpf) = VALUE ty_t_mkpf(
                    ( mblnr = '1' mjahr = '2019' cpudt = sy-datum )
                    ( mblnr = '2' mjahr = '2019' cpudt = sy-datum - 1 )
                    ( mblnr = '3' mjahr = '2019' cpudt = sy-datum - 2 )
                                   ).

    DATA(lt_mseg) =
         VALUE ty_t_mseg(
         ( mblnr = '1' mjahr = '2019' zeile = 1 menge = 1 meins = 'EA' )
         ( mblnr = '1' mjahr = '2019' zeile = 2 menge = 2 meins = 'EA' )
         ( mblnr = '1' mjahr = '2019' zeile = 3 menge = 3 meins = 'EA' )
         ( mblnr = '2' mjahr = '2019' zeile = 1 menge = 7 meins = 'BOX' )
         ( mblnr = '2' mjahr = '2019' zeile = 2 menge = 8 meins = 'BOX' )
         ( mblnr = '3' mjahr = '2019' zeile = 1 menge = 30 meins = 'EA' )
         ( mblnr = '3' mjahr = '2019' zeile = 2 menge = 40 meins = 'EA' )
         ( mblnr = '3' mjahr = '2019' zeile = 3 menge = 50 meins = 'EA' )
                        ).

    lo_output->write( lt_mkpf ).

    lo_output->write( lt_mseg ).

    lt_mseg = CORRESPONDING ty_t_mseg( lt_mseg FROM lt_mkpf
                                               USING KEY primary_key mblnr = mblnr mjahr = mjahr
                                               MAPPING create_date = cpudt ).

    lo_output->write( lt_mseg ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD internal_table.
    DATA(lo_output) = cl_demo_output=>new( ).

* LET Expressions
    lo_output->next_section( 'LET Expressions' ).

    TYPES ty_t_text TYPE STANDARD TABLE OF text80 WITH EMPTY KEY.

    DATA(lt_tom_and_jerry) = VALUE ty_t_text( LET t = 'Tom' j = 'Jerry' IN
                                              ( |There are a cat and a rat live together in a house| )
                                              ( |The cat is { t } and the rat is { j }| )
                                              ( |{ t } would like to eat { j } but { j } is very smart| )
                                            ).

    lo_output->write( lt_tom_and_jerry ).
    lo_output->line( ).

* Table Comprehensions
    lo_output->next_section( 'Table Comprehensions' ).

    TYPES:
      BEGIN OF ty_billing,
        billing_no   TYPE n LENGTH 10,
        billing_date TYPE d,
        amt          TYPE p LENGTH 16 DECIMALS 2,
        waers        TYPE waers,
      END OF ty_billing.
    TYPES ty_t_billing TYPE STANDARD TABLE OF ty_billing
                       WITH NON-UNIQUE KEY billing_no.


    DATA: lt_billing TYPE ty_t_billing,
          ls_billing TYPE ty_billing.

    DO 30 TIMES.
      ls_billing-billing_no = sy-tabix.
      ls_billing-billing_date = ( sy-datum + sy-tabix ).
      ls_billing-amt = ( CONV f( sy-tabix ) * 100 ).
      ls_billing-waers = 'THB'.
      APPEND ls_billing TO lt_billing.
      CLEAR ls_billing.
    ENDDO.
    SORT lt_billing BY billing_no.

    lo_output->write( 'Billing' ).
    lo_output->write( lt_billing ).

    lo_output->write( 'Billing in the second half of the month' ).
    DATA(lt_billing_tmp) = VALUE ty_t_billing( FOR ls_bl IN lt_billing WHERE ( billing_date > '20190715')
                                               ( ls_bl )
                                             ).
    lo_output->write( lt_billing_tmp ).

    CLEAR: lt_billing_tmp.
    lo_output->write( 'Billing from index from 30' ).
    lt_billing_tmp = VALUE ty_t_billing(
                     FOR ls_bl IN lt_billing INDEX INTO lv_idx
                     WHERE ( billing_date+6(1) = '3' )

                     ( LINES OF lt_billing FROM lv_idx ) ).
    lo_output->write( lt_billing_tmp ).

    CLEAR: lt_billing_tmp.
    lo_output->write( 'Billing with selected columns' ).
    lt_billing_tmp = VALUE ty_t_billing(
                     FOR ls_bl IN lt_billing
                     WHERE ( billing_date+6(1) = '1' )

                       ( billing_no = ls_bl-billing_no
                         amt = ls_bl-amt
                       )
                     ).
    lo_output->write( lt_billing_tmp ).

    lo_output->display( ).

  ENDMETHOD.

  METHOD open_sql.

    DATA(lo_output) = cl_demo_output=>new( ).

* OPEN SQL
    lo_output->next_section( 'OPEN SQL' ).

    lo_output->write( 'Select statement' ).

    DATA: lv_lifnr TYPE lifnr.

    DATA(lo_input) = cl_demo_input=>new( ).

* Enter vendor number
    lo_input->request( EXPORTING text = 'Vendor number'
                       CHANGING field = lv_lifnr ).

    SELECT ebeln, bsart, zekko~lifnr,
          CASE title
            WHEN 'F' THEN ( 'Miss' && ' ' && name1 && ' ' && name2 )
            WHEN 'M' THEN ( 'Mr' && ' ' && name1 && ' ' && name2 )
          END AS full_name,
          CASE waers
            WHEN 'THB' THEN 'Baht'
            WHEN 'USD' THEN 'Dollar'
          END AS currency,
         amt, waers, main_qty,
         abs( main_qty ) + abs( second_qty ) AS sum_qty,
         abs( main_qty - second_qty ) AS diff_qty,
         CAST( second_qty AS FLTP ) AS reserve_qty,
         meins
   FROM zekko INNER JOIN zlfa1
              ON zekko~lifnr = zlfa1~lifnr
   INTO TABLE @DATA(lt_result)
   WHERE zekko~lifnr = @lv_lifnr
     ORDER BY ebeln.

    lo_output->write( lt_result ).
    lo_output->line( ).

    CONSTANTS lc_po TYPE ebeln VALUE '2100015860'.

    lo_output->write( |PO Number { lc_po }| ).
    DATA(ls_result) = lt_result[ ebeln = lc_po ].
    lo_output->write( |PO no = { ls_result-ebeln } | &
                      |Sum Qty = { ls_result-sum_qty }| ).
    lo_output->line( ).


    lo_output->display( ).
  ENDMETHOD.

  METHOD meshes.

    DATA(lo_output) = cl_demo_output=>new( ).

* Meshes
    lo_output->next_section( 'Meshes' ).

    TYPES:
      t_scarr   TYPE SORTED TABLE OF scarr
                WITH UNIQUE KEY carrid,
      t_spfli   TYPE SORTED TABLE OF spfli
                WITH UNIQUE KEY carrid connid,
      t_sflight TYPE SORTED TABLE OF sflight
                WITH UNIQUE KEY carrid connid fldate.

    TYPES:
      BEGIN OF MESH t_flights,
        scarr   TYPE t_scarr
          ASSOCIATION to_spfli TO spfli
                   ON carrid = carrid USING KEY primary_key,
        spfli   TYPE t_spfli
          ASSOCIATION to_sflight TO sflight
                   ON carrid = carrid AND
                      connid = connid USING KEY primary_key,
        sflight TYPE t_sflight,
      END OF MESH t_flights.

    DATA: flights TYPE t_flights.

    DATA(root) = flights-scarr[ carrname = 'United Airlines' ].

    LOOP AT flights-scarr\to_spfli[ root ]\to_sflight[ ] INTO DATA(wa).

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_new_features_740_sp8 IMPLEMENTATION.

  METHOD main.

* Expression
*    lcl_abap_new_features_740_sp8=>expressions( ).

* Internal tables
*    lcl_abap_new_features_740_sp8=>internal_table( ).

* Database access
    lcl_abap_new_features_740_sp8=>database_access( ).
  ENDMETHOD.

  METHOD expressions.

    DATA(lo_output) = cl_demo_output=>new( ).

* New Boolean Function
    lo_output->next_section( 'Boolean Function' ).
    lo_output->write( | if boolc( 1 = 2 ) = abap_false?| ).
    lo_output->write( COND string(
                        WHEN boolc( 1 = 2 ) = abap_false  THEN 'Yes'
                        ELSE 'No'
                                 )
                    ).
    lo_output->next_section( 'New Boolean Function xsdbool' ).
    lo_output->write( |if xsdbool( 1 = 2 ) = abap_false?| ).
    lo_output->write( COND string(
                        WHEN xsdbool( 1 = 2 ) = abap_false THEN 'Yes'
                        ELSE 'No'
                                 )
                    ).

* For Expression
    lo_output->next_section( 'For Expression' ).

    TYPES ty_t_text TYPE STANDARD TABLE OF text80 WITH EMPTY KEY.
    DATA(lt_month) = VALUE ty_t_text(
                     FOR i = 01 THEN i + 1 UNTIL i > 12
                     ( |Current month is { i } Next month is { i + 1 }| )
                     ).

    lo_output->write( lt_month ).

* REDUSE
    lo_output->next_section( 'REDUCE' ).

*    DATA(lv_result) = REDUCE string( INIT text = 'Count up:'
*                      FOR n = 01 THEN n + 1 UNTIL n > 100
*                      NEXT text = text && | { n }| ).
*    lo_output->write( lv_result ).


    DATA lt_int TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    lt_int = VALUE #( FOR j = 1 WHILE j <= 10 ( j ) ).

    DATA(lv_sum) = REDUCE i( INIT x = 0 FOR lv_i IN lt_int NEXT x = x + lv_i ).
    lo_output->write( lv_sum ).



*    DATA: text TYPE string VALUE 'Test'.
*
*    DATA(chars) = VALUE stringtab( FOR off = 0 THEN off + 1
*                                   WHILE off < strlen( text )
*                                   ( |{ text+off(1) }| )
*                                 ).
*    lo_output->write( chars ).

* Base
    lo_output->next_section( 'BASE' ).

*DATA:
*  BEGIN OF struct1,
*    col1 TYPE i VALUE 11,
*    col2 TYPE i VALUE 12,
*  END OF struct1.
*
*DATA:
*  BEGIN OF struct2,
*    col2 TYPE i VALUE 22,
*    col3 TYPE i VALUE 23,
*  END OF struct2.
*
*    struct2 = CORRESPONDING #( BASE ( struct2 ) struct1 ).

    TYPES:
      BEGIN OF ty_st1,
        col1 TYPE i,
        col2 TYPE i,
      END OF ty_st1.

    TYPES:
      BEGIN OF ty_st2,
        col2 TYPE i,
        col3 TYPE i,
      END OF ty_st2.

    DATA: ls_st1 TYPE ty_st1,
          ls_st2 TYPE ty_st2.


    DO 2 TIMES.
      CLEAR: ls_st1, ls_st2.

      ls_st1 = VALUE ty_st1( col1 = 15 col2 = 20 ).
      ls_st2 = VALUE ty_st2( col2 = 17 col3 = 40 ).

      lo_output->write( 'ST1 is' ).
      lo_output->write( ls_st1 ).

      CASE sy-index.
        WHEN 1.
          lo_output->next_section( 'Corresponding without BASE').

          ls_st2 = CORRESPONDING #( ls_st1 ).
        WHEN 2.
          lo_output->next_section( 'Corresponding with BASE' ).

          ls_st2 = CORRESPONDING #( BASE ( ls_st2 ) ls_st1 ).
      ENDCASE.


      lo_output->write( 'ST2 is' ).
      lo_output->write( ls_st2 ).
      lo_output->line( ).
    ENDDO.


    lo_output->display( ).

*TYPES outref TYPE REF TO if_demo_output.
*
*DATA(output) =
*  REDUCE outref( INIT out  = cl_demo_output=>new( )
*                      text = `Count up:`
*                 FOR n = 1 UNTIL n > 11
*                 NEXT out = out->write( text )
*                      text = |{ n }| ).
*
*output->display( ).


  ENDMETHOD.

  METHOD internal_table.
    TYPES:
      BEGIN OF ty_vbap,
        vbeln TYPE vbeln,
        vbelp TYPE n LENGTH 3,
        matnr TYPE matnr,
        qty   TYPE i,
        unit  TYPE meins,
      END OF ty_vbap.
    TYPES ty_ts_vbap TYPE SORTED TABLE OF ty_vbap
                     WITH NON-UNIQUE KEY vbeln vbelp.

    DATA(lo_output) = cl_demo_output=>new( ).

* GROUP BY for internal table
    lo_output->next_section( 'GROUP BY for Internal Tables' ).

    DATA(lt_vbap) = VALUE ty_ts_vbap( ( vbeln = 1
                                        vbelp = 1
                                        matnr = 'MAT A'
                                        qty = 5
                                        unit = 'BOX'
                                      )
                                      ( vbeln = 1
                                        vbelp = 2
                                        matnr = 'MAT B'
                                        qty = 7
                                        unit = 'EA'
                                      )
                                      ( vbeln = 1
                                        vbelp = 3
                                        matnr = 'MAT C'
                                        qty = 10
                                        unit = 'TON'
                                      )
                                      ( vbeln = 2
                                        vbelp = 1
                                        matnr = 'MAT X'
                                        qty = 20
                                        unit = 'BOX'
                                      )
                                      ( vbeln = 2
                                        vbelp = 2
                                        matnr = 'MAT Y'
                                        qty = 50
                                        unit = 'EA'
                                      )
                                      ( vbeln = 2
                                        vbelp = 3
                                        matnr = 'MAT Z'
                                        qty = 80
                                        unit = 'TON'
                                      )
                                    ).

    DATA: ls_vbap_group TYPE ty_vbap.

    LOOP AT lt_vbap INTO DATA(ls_vbap)
         GROUP BY ( vbeln = ls_vbap-vbeln )
                  ASCENDING
                  ASSIGNING FIELD-SYMBOL(<group_by_billing>).

      lo_output->write( <group_by_billing> ).

*      LOOP AT GROUP <group_by_billing> ASSIGNING FIELD-SYMBOL(<vbap>).
*        lo_output->write( <vbap> ).
*      ENDLOOP.
    ENDLOOP.

* Filter expressions
    lo_output->next_section( 'Filter expressions' ).

    TYPES ty_matgrp TYPE c LENGTH 6.
    TYPES:
      BEGIN OF ty_mara,
        matnr TYPE matnr,
        maktl TYPE ty_matgrp,
        meins TYPE meins,
      END OF ty_mara.
    TYPES ty_ts_mara TYPE SORTED TABLE OF ty_mara
                     WITH NON-UNIQUE KEY matnr
                     WITH NON-UNIQUE SORTED KEY mat_grp
                     COMPONENTS maktl.

    DATA(lt_mara) =
         VALUE ty_ts_mara( ( matnr = 'M1' maktl = 'G01' meins = 'EA' )
                           ( matnr = 'M2' maktl = 'G01' meins = 'BOX' )
                           ( matnr = 'M3' maktl = 'G01' meins = 'EA' )
                           ( matnr = 'M4' maktl = 'G02' meins = 'EA' )
                           ( matnr = 'M5' maktl = 'G02' meins = 'BOX' )
                           ( matnr = 'M6' maktl = 'G02' meins = 'EA' )
                           ( matnr = 'M7' maktl = 'G03' meins = 'EA' )
                           ( matnr = 'M8' maktl = 'G03' meins = 'BOX' )
                           ( matnr = 'M9' maktl = 'G03' meins = 'EA' )
                         ).

    DATA lt_mat_grp_key TYPE STANDARD TABLE OF ty_matgrp
                        WITH EMPTY KEY.

    lt_mat_grp_key = VALUE #( ( 'G01' ) ( 'G03' ) ).

    LOOP AT lt_mat_grp_key INTO DATA(lv_mat_grp).
      lo_output->write( |Material in Mat Group { lv_mat_grp } are| ).

      DATA(lt_filter_matgrp) = FILTER #( lt_mara USING KEY mat_grp
                           WHERE maktl = lv_mat_grp ).

      lo_output->write( lt_filter_matgrp ).
      FREE: lt_filter_matgrp.
    ENDLOOP.

    lo_output->next_section( 'Filter expressions: Table' ).

    TYPES:
      BEGIN OF ty_mseg,
        mblnr TYPE c LENGTH 10,
        mjahr TYPE n LENGTH 4,
        zeile TYPE n LENGTH 4,
        bwart TYPE c LENGTH 3,
        matnr TYPE matnr,
        werks TYPE char4,
        menge TYPE i,
        meins TYPE meins,
      END OF ty_mseg.
    TYPES ty_t_mseg TYPE STANDARD TABLE OF ty_mseg
                    "Primary keys
                    WITH NON-UNIQUE KEY mblnr mjahr zeile
                    "Secondary key#1
                    WITH NON-UNIQUE SORTED KEY mat_plant
                    COMPONENTS matnr werks
                    "Secondary key#2
                    WITH NON-UNIQUE SORTED KEY mvt
                    COMPONENTS bwart.


    DATA(lt_mseg) =
      VALUE ty_t_mseg( ( mblnr = '4900147718'
                         mjahr = '2019'
                         zeile = '1'
                         bwart = 'X11'
                         matnr = '1110-30081-00003'
                         werks = '1020'
                         menge = 300
                         meins = 'EA'
                       )
                       ( mblnr = '4900147802'
                         mjahr = '2019'
                         zeile = '1'
                         bwart = '561'
                         matnr = '10151-0024-00055'
                         werks = '1020'
                         menge = 2000
                         meins = 'EA'
                       )
                       ( mblnr = '4900147803'
                         mjahr = '2019'
                         zeile = '1'
                         bwart = '601'
                         matnr = '10151-0024-00055'
                         werks = '1020'
                         menge = 1100
                         meins = 'EA'
                       )
                       ( mblnr = '4900147804'
                         mjahr = '2019'
                         zeile = '1'
                         bwart = '561'
                         matnr = '10101-0040-00443'
                         werks = '1020'
                         menge = 19
                         meins = 'EA'
                       )
                       ( mblnr = '4900147805'
                         mjahr = '2019'
                         zeile = '1'
                         bwart = '561'
                         matnr = '10101-0040-00443'
                         werks = '1020'
                         menge = 18
                         meins = 'EA'
                       )
                       ( mblnr = '4900147725'
                         mjahr = '2019'
                         zeile = '1'
                         bwart = '601'
                         matnr = '20202-0024-01114'
                         werks = '1020'
                         menge = 1
                         meins = 'EA'
                       )
                     ).

    lo_output->write( |Contents of table LT_MSEG| ).
    lo_output->write( lt_mseg ).


    TYPES:
      BEGIN OF ty_filter_mvt,
        bwart TYPE c LENGTH 3,
      END OF ty_filter_mvt.
    TYPES ty_th_filter_mvt TYPE HASHED TABLE OF ty_filter_mvt
                          WITH UNIQUE KEY bwart.

    DATA(lt_filter_mvt) = VALUE ty_th_filter_mvt(
                          ( bwart = 'X11' )
                          ( bwart = '601' ) ).

    lo_output->write( |Filter by MVT| ).
    lo_output->write( lt_filter_mvt ).

    lo_output->write( FILTER #( lt_mseg IN lt_filter_mvt
                              WHERE bwart = bwart )
                    ).


    TYPES:
      BEGIN OF ty_filter_by_mat_plant,
        matnr TYPE matnr,
        werks TYPE c LENGTH 4,
      END OF ty_filter_by_mat_plant.
    TYPES ty_th_filter_by_mat_plant TYPE HASHED TABLE OF ty_filter_by_mat_plant
          WITH UNIQUE KEY matnr werks.

    DATA(lt_filter_mat_plant) =
        VALUE ty_th_filter_by_mat_plant(
          ( matnr = '10151-0024-00055' werks = '1020')
          ( matnr = '10101-0040-00443' werks = '1020') ).

    lo_output->write( |Filter by Mat and Plant| ).
    lo_output->write( lt_filter_mat_plant ).

    lo_output->write( FILTER #( lt_mseg IN lt_filter_mat_plant
                              WHERE matnr = matnr
                                AND werks = werks )
                    ).


    lo_output->display( ).

  ENDMETHOD.

  METHOD database_access.


    DATA(lo_output) = cl_demo_output=>new( ).

    DATA: lv_mtart TYPE zmara-mtart VALUE 'ZRAW'.


    lo_output->next_section( 'Inline Declarations after INTO' ).

* Enter Material type
    cl_demo_input=>new( )->request(
      EXPORTING
        text        = 'Enter Material type'
      CHANGING
        field       = lv_mtart ).

    SELECT matnr, ersda, matkl
    FROM zmara INTO TABLE @DATA(lt_mara)
    WHERE mtart = @lv_mtart.

    lo_output->write( lt_mara ).

    SELECT ebeln, aedat, lifnr,
           CASE waers
             WHEN 'THB' THEN 'Baht'
             WHEN 'USD' THEN 'Dollar'
             ELSE 'Unknow'
           END AS currency,
           amt, waers
    FROM zekko
    INTO TABLE @DATA(lt_ekko)
    WHERE bsart IN ( 'ZF02', 'ZF02' ).

    lo_output->write( lt_ekko ).

    lo_output->display( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_new_features_750 IMPLEMENTATION.

  METHOD main.
* Fundamentals
*    lcl_abap_new_features_750=>fundamentals( ).

* Assignments and Expressions
*    lcl_abap_new_features_750=>assignments_and_expressions( ).

* Messages and Exceptions
*    lcl_abap_new_features_750=>messages_and_exceptions( ).

* Open SQL
    lcl_abap_new_features_750=>open_sql( ).
  ENDMETHOD.

  METHOD abap_cds.

  ENDMETHOD.

  METHOD assignments_and_expressions.
    TYPES:
      BEGIN OF ty_ref_data,
        data TYPE REF TO data,
      END OF ty_ref_data.

    DATA(lo_output) = cl_demo_output=>new( ).


* IS INSTANCE OF
    lo_output->next_section( 'IS INSTANCE OF' ).

    DATA: lv_bukrs   TYPE bukrs VALUE '1000',
          ls_sflight TYPE sflight,
          lt_adrc    TYPE STANDARD TABLE OF adrc WITH DEFAULT KEY.

    ls_sflight = VALUE #( mandt = sy-mandt
                          carrid = 'JAL' ).

    lt_adrc = VALUE #( ( client = sy-mandt addrnumber = '1' )
                       ( client = sy-mandt addrnumber = '2' ) ).

    DATA: lt_ref_data TYPE STANDARD TABLE OF ty_ref_data
                      WITH EMPTY KEY.

    lt_ref_data = VALUE #( ( data = REF #( lv_bukrs ) )
                           ( data = REF #( ls_sflight ) )
                           ( data = REF #( lt_adrc ) ) ).

    LOOP AT lt_ref_data INTO DATA(ls_ref_data).
      ASSIGN ls_ref_data-data->* TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data( <field> ).
      ENDIF.

      CHECK lo_typedescr IS NOT INITIAL.

      lo_output->write(
                        COND string(
                         WHEN lo_typedescr IS INSTANCE OF cl_abap_elemdescr
                              THEN 'This field is element'
                         WHEN lo_typedescr IS INSTANCE OF cl_abap_structdescr
                              THEN 'This field is structure'
                         WHEN lo_typedescr IS INSTANCE OF cl_abap_tabledescr
                              THEN 'This field is table'
                         ELSE 'I do not know'
                              )
                      ).

* Write contents
      lo_output->write( <field> ).
    ENDLOOP.

* CASE TYPE OF
    lo_output->next_section( 'CASE TYPE OF' ).

    LOOP AT lt_ref_data INTO ls_ref_data.
      ASSIGN ls_ref_data-data->* TO <field>.
      IF sy-subrc = 0.
        lo_typedescr = cl_abap_typedescr=>describe_by_data( <field> ).
      ENDIF.

      CHECK lo_typedescr IS NOT INITIAL.

      CASE TYPE OF lo_typedescr.
        WHEN TYPE cl_abap_elemdescr INTO DATA(lo_elemdescr).
          lo_output->write( lo_elemdescr->get_ddic_field( sy-langu ) ).
        WHEN TYPE cl_abap_structdescr INTO DATA(lo_structdescr).
          lo_output->write(
                     lo_structdescr->get_ddic_field_list( p_langu = sy-langu )
                          ).
        WHEN TYPE cl_abap_tabledescr INTO DATA(lo_tabledescr).
          lo_output->write(
                     lo_tabledescr->get_table_line_type( )->get_ddic_object( )
                          ).
      ENDCASE.

      FREE: lo_typedescr.
    ENDLOOP.

* CL_ABAP_CORRESPONDING
    lo_output->next_section( 'CL_ABAP_CORRESPONDING' ).

    DATA:
      BEGIN OF struct1,
        a1 TYPE string VALUE 'a1',
        a2 TYPE string VALUE 'a2',
        a3 TYPE string VALUE 'a3',
        a4 TYPE string VALUE 'a4',
        a5 TYPE string VALUE 'a5',
      END OF struct1,

      BEGIN OF struct2,
        b1 TYPE string VALUE 'b1',
        b2 TYPE string VALUE 'b2',
        b3 TYPE string VALUE 'b3',
        a4 TYPE string VALUE 'b4',
        a5 TYPE string VALUE 'b5',
      END OF struct2.

    lo_output->write( 'Before moving' ).
    lo_output->write( struct1 ).
    lo_output->write( struct2 ).

    DATA(lo_mapper) =
      cl_abap_corresponding=>create(
        source            = struct1
        destination       = struct2
        mapping           = VALUE cl_abap_corresponding=>mapping_table(
          ( level = 0 kind = 1 srcname = 'a1' dstname = 'b3' )
          ( level = 0 kind = 1 srcname = 'a3' dstname = 'b1' )
          ( level = 0 kind = 2 srcname = 'a4' ) ) ).
*      ( level = 0 kind = 3 ) ) ).

    lo_mapper->execute( EXPORTING source      = struct1
                     CHANGING  destination = struct2 ).
    lo_output->write( 'After Moved' ).
    lo_output->write( struct2 ).

    TYPES:
      BEGIN OF ty_fi_doc,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        budat TYPE budat,
        cpudt TYPE sydatum,
*        aedat TYPE aedat,
      END OF ty_fi_doc.


    DATA: lt_fi_doc TYPE STANDARD TABLE OF ty_fi_doc
                    WITH DEFAULT KEY.

    lt_fi_doc = VALUE #( ( bukrs = '1000'
                           belnr = '1'
                           gjahr = sy-datum+0(4)
                           budat = sy-datum
                           cpudt = sy-datum - 2 )
                         ( bukrs = '1000'
                           belnr = '2'
                           gjahr = sy-datum+0(4)
                           budat = sy-datum
                           cpudt = sy-datum - 3 )
                         ( bukrs = '1000'
                           belnr = '3'
                           gjahr = sy-datum+0(4)
                           budat = sy-datum + 1
                           cpudt = sy-datum )
                         ( bukrs = '1000'
                           belnr = '4'
                           gjahr = sy-datum+0(4)
                           budat = sy-datum
                           cpudt = sy-datum - 1 )
                       ).

    lo_output->write( lt_fi_doc ).

    TYPES:
      BEGIN OF ty_rpt,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        date  TYPE sydatum,
      END OF ty_rpt.

    DATA: lt_rpt_a TYPE STANDARD TABLE OF ty_rpt,
          lt_rpt_b TYPE STANDARD TABLE OF ty_rpt.
    DATA lt_mapping_table TYPE cl_abap_corresponding=>mapping_table.


    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lt_mapping_table = VALUE #(
                                      ( level = 0
                                        kind = 1
                                        srcname = 'budat'
                                        dstname = 'date' )
                                    ).
          lo_mapper = cl_abap_corresponding=>create(
                        source = lt_fi_doc
                        destination = lt_rpt_a
                        mapping = lt_mapping_table ).

          TRY.
              lo_mapper->execute(
                EXPORTING
                  source            = lt_fi_doc
                CHANGING
                  destination       = lt_rpt_a ).

              lo_output->write( lt_rpt_a ).
            CATCH cx_corr_dyn_error.
          ENDTRY.
        WHEN 2.
      ENDCASE.
    ENDDO.

    lo_output->display( ).
  ENDMETHOD.

  METHOD external_interfaces.

  ENDMETHOD.

  METHOD fundamentals.
    DATA(lo_output) = cl_demo_output=>new( ).

    lo_output->next_section( 'Only Unicode Systems in Release 7.50' ).

    lo_output->next_section( 'New Built-in Data Type INT8' ).

    DATA arg TYPE int8 VALUE 2.

    lo_output->write( |**  : { arg ** 62 }| ).
    lo_output->write( |ipow: { ipow( base = arg exp = 62 ) }| ).

    lo_output->write( |Note:| &&
                      |** calculates with floating point| &&
                      |built-in fm ipow calculates with int8| ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD messages_and_exceptions.
    DATA: lt_data TYPE STANDARD TABLE OF text40.

** IF_T100_MESSAGE in Regular Class
*    lcl_using_system_interface_msg=>main( ).

    DATA(lo_output) = cl_demo_output=>new( ).

* IF_T100_MESSAGE for Exception with Message
    lo_output->next_section( 'IF_T100_MESSAGE for Exception with Message' ).

    TRY.
        cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = 'D:\test.txt'
          CHANGING
            data_tab                = lt_data
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
            OTHERS                  = 19
        ).
        IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

          RAISE EXCEPTION TYPE cx_demo_t100
            MESSAGE ID sy-msgid
            NUMBER sy-msgno
            EXPORTING
              text1 = CONV #( sy-msgv1 )
              text2 = CONV #( sy-msgv2 )
              text3 = CONV #( sy-msgv3 )
              text4 = CONV #( sy-msgv4 ).
        ENDIF.
      CATCH cx_demo_t100 INTO DATA(lx_ref).

        lo_output->write( |Caught exception:\n\n| &&
                          |"{ lx_ref->get_text( ) }"| ).

        MESSAGE lx_ref TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    lo_output->display( ).
  ENDMETHOD.

  METHOD open_sql.

    DATA(lo_output) = cl_demo_output=>new( ).

* Host Expressions
    lo_output->next_section( 'Host Expressions' ).


    DATA lv_name TYPE scarr-carrname VALUE 'United Airlines'.
    DATA carriers TYPE HASHED TABLE OF scarr
                  WITH UNIQUE KEY carrid
                  WITH NON-UNIQUE SORTED KEY name COMPONENTS carrname.

* Get Airline company
    SELECT *
    FROM scarr INTO TABLE carriers.

    cl_demo_input=>request( CHANGING field = lv_name ).


* Get from Flight
    SELECT carrid, connid, cityfrom, cityto
    FROM spfli
    WHERE carrid =
          @( VALUE spfli-carrid( carriers[ KEY name
                                               carrname = lv_name ]-carrid
                                               OPTIONAL ) )
    INTO TABLE @DATA(lt_result).

    lo_output->write( lt_result ).


    SELECT carrid, connid, fldate, seatsmax, seatsocc,
           seatsmax - seatsocc AS seatsfree
    FROM sflight
    WHERE seatsmax - seatsocc > 80
    INTO TABLE @DATA(lt_check_seat).

    lo_output->write( lt_check_seat ).


    CONSTANTS: lc_1hour_in_sec TYPE i VALUE 3600.

    DATA(lv_current_time) = CONV t( sy-uzeit + ( lc_1hour_in_sec * 6 ) ).

    lo_output->write( |Current time is { lv_current_time TIME = USER }| ).
    lo_output->write( |Depart from { CONV t( ( lv_current_time - ( lc_1hour_in_sec * 2 ) ) ) TIME = USER } | &
                      |To { CONV t( lv_current_time + ( lc_1hour_in_sec * 2 ) ) TIME = USER } +- 2 Hours | ).

    SELECT carrid,
           cityfrom && '-' && cityto AS route,
           deptime, arrtime
    FROM spfli
    WHERE deptime BETWEEN @( CONV t( lv_current_time - ( lc_1hour_in_sec * 2 ) ) )
                      AND @( CONV t( lv_current_time + ( lc_1hour_in_sec * 2 ) ) )
    ORDER BY deptime ASCENDING
    INTO TABLE @DATA(lt_spfli).

    lo_output->write( lt_spfli ).

* Union
    DATA prog_range TYPE RANGE OF trdir-name.
    DATA:devclass TYPE devclass VALUE '$TMP'.
    DATA: lr_obj_name TYPE RANGE OF tadir-obj_name.

    lr_obj_name = VALUE #( ( sign = 'I' option = 'CP' low = 'Z*' )
                           ( sign = 'I' option = 'CP' low = 'SAPLZ*' ) ).

    SELECT 'I' AS sign, 'EQ' AS option, obj_name AS low, ' ' AS high
           FROM tadir
           WHERE pgmid = 'R3TR'
             AND object = 'PROG'
             AND obj_name IN @lr_obj_name
             AND devclass = @devclass
    UNION
    SELECT 'I' AS sign, 'CP' AS option, obj_name && '*' AS low, ' ' AS high
            FROM tadir
            WHERE pgmid = 'R3TR'
              AND object = 'CLAS'
              AND obj_name IN @lr_obj_name
              AND devclass = @devclass
    UNION
    SELECT 'I' AS sign, 'EQ' AS option, 'SAPL' && obj_name AS low, ' ' AS high
           FROM tadir
           WHERE pgmid = 'R3TR'
             AND object = 'FUGR'
             AND obj_name IN @lr_obj_name
             AND devclass = @devclass
    UNION
    SELECT 'I' AS sign, 'CP' AS option, 'L' && obj_name && '+++' AS low, ' ' AS high
           FROM tadir
           WHERE pgmid = 'R3TR'
             AND object = 'FUGR'
             AND obj_name IN @lr_obj_name
             AND devclass = @devclass
           INTO TABLE @prog_range.

    lo_output->write( prog_range ).

    TYPES:
      BEGIN OF ty_partner,
        partner_no  TYPE lifnr,
        parter_type TYPE char10,
        fullname    TYPE string,
      END OF ty_partner.
    TYPES ty_t_partner TYPE STANDARD TABLE OF ty_partner
                       WITH NON-UNIQUE KEY partner_no.

    lo_output->next_section( 'Union' ).


    DATA: lt_partner TYPE ty_t_partner.

    SELECT lifnr AS partner,
           'VEND' AS partner_type,
           name1 && ' ' && name2 AS fullname
    FROM zlfa1
    WHERE title = 'F'
    UNION
    SELECT kunnr AS partner,
           'CUST' AS partner_type,
           name1 && ' ' && name2 AS fullname
    FROM zkna1
    WHERE title = 'F'
    INTO TABLE @lt_partner.

    lo_output->write( lt_partner ).

* INSERT FROM Subquery
    lo_output->next_section( 'INSERT FROM Subquery' ).

    INSERT demo_sumdist_agg FROM
    ( SELECT
      FROM scarr AS s
      INNER JOIN spfli AS p ON s~carrid = p~carrid
* Field Options: selected fieldnames can be listed after from
      FIELDS s~carrname,
             p~distid,
             SUM( p~distance ) AS sum_distance
      GROUP BY s~mandt, s~carrname, p~distid ).

    SELECT *
    FROM demo_sumdist_agg INTO TABLE @DATA(lt_tmp_sum).
    lo_output->write( lt_tmp_sum ).

    DELETE FROM demo_sumdist_agg.


    lo_output->display( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_new_features_751 IMPLEMENTATION.

  METHOD main.

* Data Type
*    data_types( ).

* Assignment
*    assignments( ).

* ABAP CDS and Open SQL
    abap_cds_and_open_sql( ).

* Further News
    further_news( ).
  ENDMETHOD.

  METHOD abap_cds_and_open_sql.
    DATA(lo_output) = cl_demo_output=>new( ).

* Common Table Expressions in Open SQL (CTE)
    lo_output->next_section( 'Common Table Expressions in Open SQL' ).

    DATA: lr_name TYPE RANGE OF zlfa1-name1.

    lr_name = VALUE #( ( sign = 'I' option = 'EQ' low = 'Anna')
                       ( sign = 'I' option = 'EQ' low = 'Luke')
                       ( sign = 'I' option = 'EQ' low = 'Thomas') ).

    WITH
      +vendor AS (
        SELECT lifnr,
               name1 && ' ' && name2 AS fullname
               FROM zlfa1
               WHERE name1 IN @lr_name ),
      +sum_po AS (
        SELECT zekko~lifnr, zterm,
               SUM( amt ) AS amount,
               waers
        FROM zekko
        WHERE lifnr IN ( SELECT lifnr
                         FROM +vendor )
        GROUP BY zekko~lifnr, zterm, waers )

      SELECT v~lifnr,
             fullname,
             zterm,
             amount,
             waers
      FROM +vendor AS v INNER JOIN +sum_po AS s
                   ON v~lifnr = s~lifnr
      ORDER BY v~lifnr
      INTO TABLE @DATA(lt_result).

    lo_output->write( lt_result ).

* New String and other functions
    lo_output->next_section( 'New String and other functions' ).

    SELECT zdummy_ebook_h~title,
           keywords,
           upper( keywords ) AS u_keywords,
           lower( keywords ) AS l_keywords,
           concat_with_space( 'the keywords are ', keywords, 1 ) AS full_keywords,
           zdummy_ebook_i~context
    FROM zdummy_ebook_h INNER JOIN zdummy_ebook_i
                        ON zdummy_ebook_h~book_id = zdummy_ebook_i~book_id
    WHERE zdummy_ebook_h~author = 'Alexandre Dumas'
    INTO TABLE @DATA(lt_ebook).

    lo_output->write( lt_ebook ).

* Cross Join in ABAP CDS and Open SQL
    lo_output->next_section( 'Cross Join in ABAP CDS and Open SQL' ).

    WITH

      +mat AS ( SELECT matnr, matkl,
                       ' ' AS meinh
                FROM zmara
                WHERE matnr = '10151-0024-00059' ),

      +unit AS ( SELECT msehi, isocode
                 FROM t006
                 WHERE dimid = 'POWER'
                )

      SELECT matnr, matkl, +unit~msehi, isocode
      FROM +mat CROSS JOIN +unit
      INTO TABLE @DATA(lt_mat_unit).

    lo_output->write( lt_mat_unit ).

* Date and Time functions
    lo_output->next_section( 'Date and Time Functions').

    SELECT mblnr, mjahr, blart, budat, bldat,
           dats_days_between( budat, bldat ) AS diff_posting_days,
           dats_add_days( budat, 5 ) AS next_5days
    FROM zmkpf
    INTO TABLE @DATA(lt_mkpf)
    UP TO 90 ROWS.

    lo_output->write( lt_mkpf ).

    lo_output->display( ).

  ENDMETHOD.

  METHOD assignments.
    DATA(lo_output) = cl_demo_output=>new( ).

* CL_ABAP_CORRESPONDING
    lo_output->next_section( 'CL_ABAP_CORRESPONDING' ).
    TYPES:
      BEGIN OF names,
        n1 TYPE c LENGTH 2,
        n2 TYPE c LENGTH 2,
        n3 TYPE c LENGTH 2,
      END OF names.

    DATA:
      BEGIN OF struct1,
        a1 TYPE string VALUE 'a1',
        a2 TYPE string VALUE 'a2',
        a3 TYPE string VALUE 'a3',
      END OF struct1,
      BEGIN OF struct2,
        b1 TYPE string VALUE 'b1',
        b2 TYPE string VALUE 'b2',
        b3 TYPE string VALUE 'b3',
      END OF struct2.

    lo_output->write( struct1 ).
    lo_output->write( struct2 ).

    DATA(src) = VALUE names( n1 = 'a1' n2 = 'a2' n3 = 'a3').
    DATA(dst) = VALUE names( n1 = 'b3' n2 = 'b2' n3 = 'b1').

    TRY.
        DATA(mapper) =
          cl_abap_corresponding=>create(
            source      = struct1
            destination = struct2
            mapping     = VALUE cl_abap_corresponding=>mapping_table(
              ( level = 0 kind = 1 srcname = src-n1 dstname = dst-n1 )
              ( level = 0 kind = 1 srcname = src-n2 dstname = dst-n2 )
              ( level = 0 kind = 1 srcname = src-n3 dstname = dst-n3 )
            ) ).

        mapper->execute( EXPORTING source      = struct1
                         CHANGING  destination = struct2 ).
      CATCH cx_corr_dyn_error INTO DATA(exc).
        cl_demo_output=>display( exc->get_text( ) ).
    ENDTRY.
    lo_output->write( struct2 ).

* Reflexive Assignments
    lo_output->next_section( 'Reflexive Assignments' ).

    DATA:BEGIN OF str1,
           a1 TYPE string VALUE 'F1',
           a2 TYPE string VALUE 'F2',
           a3 TYPE string VALUE 'F3',
           a4 TYPE string VALUE 'F4',
         END OF str1.

    lo_output->write( str1 ).

    cl_abap_corresponding=>create(
        source      = str1
        destination = str1
        mapping     = VALUE cl_abap_corresponding=>mapping_table(
          ( level = 0 kind = 1 srcname = 'a4' dstname = 'a1' )
          ( level = 0 kind = 1 srcname = 'a3' dstname = 'a2' )
          ( level = 0 kind = 1 srcname = 'a1' dstname = 'a3' )
          ( level = 0 kind = 1 srcname = 'a2' dstname = 'a4' ) )
        )->execute( EXPORTING source      = str1
                    CHANGING  destination = str1 ).

    lo_output->write( str1 ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD data_types.

    DATA(lo_output) = cl_demo_output=>new( ).


    DATA(shirt_xl) = NEW shirt( shirt=>size_xl ).

    lo_output->write( shirt_xl->get_price( ) ).
*    DATA(shirt_xx) = NEW shirt( 333 ).

    lo_output->display( ).
  ENDMETHOD.

  METHOD further_news.
    DATA(lo_output) = cl_demo_output=>new( ).


  ENDMETHOD.

ENDCLASS.
