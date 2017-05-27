*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_ANPOP.
*----------------------------------------------------------------------*

CLASS lcl_annotation_pick_screen DEFINITION
  INHERITING FROM cl_bus_abstract_main_screen
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_dynpro_binding DATA VALUES gv_dynnr = '0101'.
    METHODS:
      constructor IMPORTING iv_program_name  TYPE bus_screen-program_name
                            iv_dynpro_number TYPE bus_screen-dynpro_number,
      pbo_begin REDEFINITION,
      get_chosen_annotation RETURNING VALUE(rv_classname) TYPE abap_classname.
  PROTECTED SECTION.
    METHODS:
      call_screen REDEFINITION,
      call_screen_starting_at REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      gc_container_name TYPE fieldname VALUE 'CC_CONTAINER',
      gc_fc_cancel      TYPE bu_fcode VALUE 'CANCEL',
      gc_fc_continue    TYPE bu_fcode VALUE 'CONTINUE',
      gc_status_0101    TYPE sypfkey VALUE 'STATUS_0101'.
    METHODS:
      handle_pai FOR EVENT process_after_input OF cl_bus_abstract_main_screen
        IMPORTING iv_function_code,
      update_tree,
      on_annotation_double_click FOR EVENT annotation_double_click OF lcl_annotation_tree
        IMPORTING classname.
    DATA:
      mo_container         TYPE REF TO cl_gui_custom_container,
      mo_annotation_tree   TYPE REF TO lcl_annotation_tree,
      mv_chosen_annotation TYPE abap_classname.
ENDCLASS.

CLASS lcl_annotation_pick_screen IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_program_name  = iv_program_name
                        iv_dynpro_number = iv_dynpro_number ).

    set_status( iv_status_key = gc_status_0101 ).
    set_title( 'Pick an annotation'(014) ).

    SET HANDLER handle_pai FOR me.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN iv_dynpro_number.
  ENDMETHOD.

  METHOD call_screen_starting_at.
    CALL SCREEN iv_dynpro_number
         STARTING AT iv_xstart iv_ystart
         ENDING AT iv_xend iv_yend.
  ENDMETHOD.

  METHOD pbo_begin.
    IF mo_annotation_tree IS NOT BOUND.
      mo_container = NEW cl_gui_custom_container( gc_container_name ).
      mo_annotation_tree = NEW lcl_annotation_tree( mo_container ).
      SET HANDLER on_annotation_double_click FOR mo_annotation_tree.
      update_tree( ).
    ENDIF.

    mo_annotation_tree->display( ).

    super->pbo_begin( ).
  ENDMETHOD.

  METHOD handle_pai.
    CASE iv_function_code.
      WHEN gc_fc_continue.
        mv_chosen_annotation = mo_annotation_tree->get_selected_annotation( ).
        leave( ).
      WHEN gc_fc_cancel.
        leave( ).
    ENDCASE.
  ENDMETHOD.

  METHOD update_tree.
    DATA: lo_annotation_dummy TYPE REF TO zcl_aap_annotation_base.

    mo_annotation_tree->update_content(
      zcl_aap_tools=>get_subclasses_of_class(
        CONV #( zcl_aap_tools=>get_objectdescr_from_data( lo_annotation_dummy
                  )->get_relative_name( ) )
      )
    ).
  ENDMETHOD.

  METHOD get_chosen_annotation.
    rv_classname = mv_chosen_annotation.
  ENDMETHOD.

  METHOD on_annotation_double_click.
    mv_chosen_annotation = classname.
    leave( ).
  ENDMETHOD.
ENDCLASS.
