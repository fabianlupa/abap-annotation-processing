*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_ANTRE.
*----------------------------------------------------------------------*

CLASS lcl_annotation_tree DEFINITION INHERITING FROM lcl_view_base.
  PUBLIC SECTION.
    EVENTS:
      update_requested,
      annotation_double_click EXPORTING VALUE(classname) TYPE abap_classname.
    METHODS:
      constructor IMPORTING io_container TYPE REF TO cl_gui_container
                  RAISING   cx_salv_error,
      update_content IMPORTING it_annotations TYPE zcl_aap_tools=>gty_class_tab
                     RAISING   cx_salv_error,
      display REDEFINITION,
      get_selected_annotation RETURNING VALUE(rv_classname) TYPE abap_classname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_devclass,
        devclass TYPE devclass,
        nodekey  TYPE salv_de_node_key,
      END OF gty_devclass,
      gty_devclass_tab TYPE HASHED TABLE OF gty_devclass WITH UNIQUE KEY devclass,
      BEGIN OF gty_class,
        classname TYPE abap_classname,
      END OF gty_class.
    CONSTANTS:
      gc_fc_refresh TYPE salv_de_function VALUE 'REFRESH'.
    CLASS-METHODS:
      add_package_nodes_recursively IMPORTING iv_devclass   TYPE devclass
                                              io_tree       TYPE REF TO cl_salv_tree
                                    CHANGING  ct_devclasses TYPE gty_devclass_tab
                                    RAISING   cx_salv_error.
    METHODS:
      on_added_function FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_double_click FOR EVENT double_click OF cl_salv_events_tree
        IMPORTING node_key,
      on_drag_multiple FOR EVENT on_drag_multiple OF cl_salv_gui_tree
        IMPORTING drag_drop_object fieldname node_key_table,
      update_drag_drop_nodes.
    DATA:
      mo_tree    TYPE REF TO cl_salv_tree,
      mt_classes TYPE STANDARD TABLE OF gty_class.
ENDCLASS.

CLASS lcl_annotation_tree IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_container ).

    cl_salv_tree=>factory(
      EXPORTING
        r_container   = io_container
      IMPORTING
        r_salv_tree   = mo_tree
      CHANGING
        t_table       = mt_classes
    ).

    mo_tree->get_functions(:
      )->set_collapse( abap_true ),
      )->set_expand( abap_true ),
      )->set_find( abap_true ),
      )->set_print_all( abap_true ),
      )->set_print_all_preview( abap_true ),
      )->set_print_view( abap_true ),
      )->set_print_view_preview( abap_true ).

    mo_tree->get_functions( )->add_function(
        name     = gc_fc_refresh
        icon     = CONV #( icon_refresh )
        tooltip  = CONV #( 'Refresh'(002) )
        position = if_salv_c_function_position=>right_of_salv_functions
    ).

    mo_tree->get_tree_settings( )->set_hierarchy_header( 'Available annotations'(006) ).

    mo_tree->get_columns( )->get_column( 'CLASSNAME' )->set_technical( ).

    SET HANDLER on_added_function FOR mo_tree->get_event( ).
    SET HANDLER on_double_click FOR mo_tree->get_event( ).
    SET HANDLER on_drag_multiple FOR lcl_salv_backdoor=>get_alv_from_salv( mo_tree ).
  ENDMETHOD.

  METHOD display.
    mo_tree->display( ).
  ENDMETHOD.

  METHOD get_selected_annotation.
    DATA(lt_selected) = mo_tree->get_selections( )->get_selected_nodes( ).
    IF lines( lt_selected ) = 1.
      rv_classname = CAST gty_class( lt_selected[ 1 ]-node->get_data_row( ) )->classname.
    ENDIF.
  ENDMETHOD.

  METHOD update_content.
    DATA: lt_devclasses TYPE gty_devclass_tab.

    mo_tree->get_nodes( )->delete_all( ).

    LOOP AT it_annotations ASSIGNING FIELD-SYMBOL(<lv_annotation>).
      " Get annotation class package
      DATA(lv_devclass) = zcl_aap_tools=>get_devclass_for_class( <lv_annotation> ).

      " Check if the node for it exists
      IF NOT line_exists( lt_devclasses[ devclass = lv_devclass ] ).
        add_package_nodes_recursively(
          EXPORTING
            iv_devclass   = lv_devclass
            io_tree       = mo_tree
          CHANGING
            ct_devclasses = lt_devclasses
        ).
      ENDIF.

      mo_tree->get_nodes( )->add_node(
        related_node   = lt_devclasses[ devclass = lv_devclass ]-nodekey
        relationship   = if_salv_c_node_relation=>last_child
        data_row       = VALUE gty_class( classname = <lv_annotation> )
        collapsed_icon = CONV #( icon_oo_class )
        expanded_icon  = CONV #( icon_oo_class )
        text           = CONV #( <lv_annotation> )
      ).
    ENDLOOP.

    mo_tree->get_nodes( )->expand_all( ).

    update_drag_drop_nodes( ).
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_salv_function.
      WHEN gc_fc_refresh.
        RAISE EVENT update_requested.
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    TRY.
        DATA(lr_row) = CAST gty_class(
                         mo_tree->get_nodes( )->get_node( node_key )->get_data_row( )
                       ).
        IF lr_row->classname IS NOT INITIAL.
          RAISE EVENT annotation_double_click EXPORTING classname = lr_row->classname.
        ENDIF.
      CATCH cx_salv_msg ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD on_drag_multiple.
    DATA: lt_classes TYPE lcl_annotation_drop=>gty_annotation_tab.

    LOOP AT node_key_table ASSIGNING FIELD-SYMBOL(<lv_node_key>).
      TRY.
          DATA(lr_data) = CAST gty_class(
                            mo_tree->get_nodes( )->get_node( <lv_node_key> )->get_data_row( )
                          ).
        CATCH cx_salv_msg cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.
      IF lr_data->classname IS NOT INITIAL.
        APPEND lr_data->classname TO lt_classes.
      ENDIF.
    ENDLOOP.

    drag_drop_object->object = NEW lcl_annotation_drop( lt_classes ).
  ENDMETHOD.

  METHOD add_package_nodes_recursively.
    " Get parent package
    DATA(lv_parent_devclass) = zcl_aap_tools=>get_parent_devclass( iv_devclass ).

    " If there is a parent package it needs to be added first, if it does not exist yet
    IF lv_parent_devclass IS NOT INITIAL
        AND NOT line_exists( ct_devclasses[ devclass = lv_parent_devclass ] ).

      add_package_nodes_recursively(
        EXPORTING
          iv_devclass   = lv_parent_devclass
          io_tree       = io_tree
        CHANGING
          ct_devclasses = ct_devclasses
      ).
    ENDIF.

    " Add the package node
    DATA(lo_node) = io_tree->get_nodes( )->add_node(
        related_node   = COND #( WHEN lv_parent_devclass IS INITIAL THEN space
                                 ELSE ct_devclasses[ devclass = lv_parent_devclass ]-nodekey )
        relationship   = if_salv_c_node_relation=>last_child
        data_row       = VALUE gty_class( )
        collapsed_icon = CONV #( icon_package_standard )
        expanded_icon  = CONV #( icon_package_standard )
        text           = CONV #( iv_devclass )
    ).
    INSERT VALUE #( devclass = iv_devclass nodekey = lo_node->get_key( ) ) INTO TABLE ct_devclasses.
  ENDMETHOD.

  METHOD update_drag_drop_nodes.
    DATA(lo_alv) = lcl_salv_backdoor=>get_alv_from_salv( mo_tree ).

    DATA(lo_dragdrop) = NEW cl_dragdrop( ).
    lo_dragdrop->add(
      flavor     = 'LCL_ANNOTATION_DROP'
      dragsrc    = abap_true
      droptarget = abap_false
      effect     = cl_dragdrop=>move + cl_dragdrop=>copy
    ).
    lo_dragdrop->get_handle( IMPORTING handle = DATA(lv_handle) ).

    TRY.
        LOOP AT mo_tree->get_nodes( )->get_all_nodes( ) ASSIGNING FIELD-SYMBOL(<ls_node>).
          DATA(lr_data) = CAST gty_class( <ls_node>-node->get_data_row( ) ).
          IF lr_data->classname IS NOT INITIAL.
            lcl_alv_helper=>add_drag_drop_to_node(
              iv_node_key   = <ls_node>-key
              iv_dragdropid = lv_handle
              io_alv        = lo_alv
            ).
          ENDIF.
        ENDLOOP.
      CATCH cx_salv_msg INTO DATA(lx_ex).
        MESSAGE lx_ex TYPE 'E'.
    ENDTRY.

    lo_alv->frontend_update( ).
  ENDMETHOD.
ENDCLASS.
