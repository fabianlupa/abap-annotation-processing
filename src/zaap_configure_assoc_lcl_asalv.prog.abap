*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_ASALV.
*----------------------------------------------------------------------*

CLASS lcl_assoc_annotations_alv DEFINITION INHERITING FROM lcl_view_base.
  PUBLIC SECTION.
    EVENTS:
      add_annotation_requested EXPORTING  VALUE(component)       TYPE zaap_l_componentid
                                          VALUE(componenttype)   TYPE zaap_l_componenttype
                                          VALUE(parentcomponent) TYPE zaap_l_parcompid
                                          VALUE(annotation) TYPE abap_classname.
    METHODS:
      constructor IMPORTING io_container TYPE REF TO cl_gui_container
                            iv_mode      TYPE lcl_const=>gty_mode DEFAULT lcl_const=>gc_mode_view
                  RAISING   cx_salv_error,
      display REDEFINITION,
      update_content IMPORTING iv_component       TYPE zaap_l_componentid OPTIONAL
                               iv_componenttype   TYPE zaap_l_componenttype OPTIONAL
                               iv_parentcomponent TYPE zaap_l_parcompid OPTIONAL,
      update_processor IMPORTING io_processor TYPE REF TO zcl_aap_proc_object,
      set_mode IMPORTING iv_new_mode      TYPE lcl_const=>gty_mode
                         iv_force_display TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_fc_remove_annotation TYPE salv_de_function VALUE 'REMOVE',
      gc_fc_edit_annotation   TYPE salv_de_function VALUE 'EDIT',
      gc_fc_add_annotation    TYPE salv_de_function VALUE 'ADD'.
    METHODS:
      on_added_function FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
    DATA:
      mo_alv                TYPE REF TO cl_salv_table,
      mt_object_annotations TYPE STANDARD TABLE OF zaap_s_as_annotation,
      mo_object_processor   TYPE REF TO zcl_aap_proc_object,
      mv_mode               TYPE lcl_const=>gty_mode,
      mv_component          TYPE zaap_l_componentid,
      mv_componenttype      TYPE zaap_l_componenttype,
      mv_parentcomponent    TYPE zaap_l_parcompid.
ENDCLASS.

CLASS lcl_assoc_annotations_alv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_container ).

    cl_salv_table=>factory(
      EXPORTING
        r_container  = io_container
      IMPORTING
        r_salv_table = mo_alv
      CHANGING
        t_table      = mt_object_annotations
    ).

    mo_alv->get_functions( )->add_function(
      name               = gc_fc_add_annotation
      icon               = CONV #( icon_add_row )
      tooltip            = CONV #( 'Add annotation'(013) )
      position           = if_salv_c_function_position=>left_of_salv_functions
    ).
    mo_alv->get_functions( )->add_function(
      name               = gc_fc_remove_annotation
      icon               = CONV #( icon_remove_row )
      tooltip            = CONV #( 'Remove annotation'(011) )
      position           = if_salv_c_function_position=>left_of_salv_functions
    ).
    mo_alv->get_functions( )->add_function(
      name               = gc_fc_edit_annotation
      icon               = CONV #( icon_modify )
      tooltip            = CONV #( 'Edit annotation'(012) )
      position           = if_salv_c_function_position=>left_of_salv_functions
    ).

    mo_alv->get_functions( )->set_all( ).

    SET HANDLER me->on_added_function FOR mo_alv->get_event( ).

    mo_alv->get_display_settings(:
      )->set_list_header( 'Associated annotations'(007) ),
      )->set_list_header_size( 1
    ).

    set_mode( iv_new_mode = iv_mode iv_force_display = abap_false ).
  ENDMETHOD.

  METHOD display.
    mo_alv->display( ).
  ENDMETHOD.

  METHOD update_content.
    DATA: lt_annotations TYPE zif_aap_annotation_resolver=>gty_annotation_tab.

    IF mo_object_processor IS NOT BOUND OR iv_component IS NOT INITIAL.
      CLEAR mt_object_annotations.
      mo_alv->refresh( ).
      RETURN.
    ENDIF.

    mv_component = iv_component.
    mv_componenttype = iv_componenttype.
    mv_parentcomponent = iv_parentcomponent.

    CASE iv_componenttype.
      WHEN space. " Class
        lt_annotations = mo_object_processor->get_annotations( ).
      WHEN 'M'. " Method
        lt_annotations = mo_object_processor->get_method_processor( CONV #( iv_component )
                           )->get_annotations( ).
      WHEN 'P'. " Parameter
        lt_annotations = mo_object_processor->get_method_processor(
                           CONV #( iv_parentcomponent )
                           )->get_parameter_processor( CONV #( iv_component )
                           )->get_annotations( ).
      WHEN 'A'. " Attribute
        lt_annotations = mo_object_processor->get_attribute_processor( CONV #( iv_component )
                           )->get_annotations( ).
    ENDCASE.

    CLEAR mt_object_annotations.

    LOOP AT lt_annotations ASSIGNING FIELD-SYMBOL(<ls_annotation>).
      APPEND VALUE #( annoclass = <ls_annotation>-classname ) TO mt_object_annotations.
    ENDLOOP.

    mo_alv->refresh( ).
  ENDMETHOD.

  METHOD update_processor.
    mo_object_processor = io_processor.
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_salv_function.
      WHEN gc_fc_add_annotation.
        DATA(lo_popup) = CAST lcl_annotation_pick_screen(
                           lcl_bus_tools=>get_screen(
                             lcl_annotation_pick_screen=>lif_dynpro_binding~gv_dynnr
                           )
                         ).
        lo_popup->show_as_popup( ).

        DATA(lv_new_annotation) = lo_popup->get_chosen_annotation( ).
        IF lv_new_annotation IS NOT INITIAL.
          RAISE EVENT add_annotation_requested
            EXPORTING
              component       = mv_component
              componenttype   = mv_componenttype
              parentcomponent = mv_parentcomponent
              annotation      = lv_new_annotation.
        ENDIF.

      WHEN gc_fc_edit_annotation.

      WHEN gc_fc_remove_annotation.

    ENDCASE.
  ENDMETHOD.

  METHOD set_mode.
    IF iv_new_mode <> mv_mode.
      mv_mode = iv_new_mode.
      DATA(lv_enabled) = COND #( WHEN mv_mode = lcl_const=>gc_mode_edit THEN abap_true ).
      TRY.
          mo_alv->get_functions( )->enable_function(:
            name = gc_fc_add_annotation boolean = lv_enabled ),
            name = gc_fc_edit_annotation boolean = lv_enabled ),
            name = gc_fc_remove_annotation boolean = lv_enabled
          ).
          IF iv_force_display = abap_true.
            mo_alv->display( ).
          ENDIF.
        CATCH cx_salv_wrong_call cx_salv_not_found INTO DATA(lx_ex) ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
