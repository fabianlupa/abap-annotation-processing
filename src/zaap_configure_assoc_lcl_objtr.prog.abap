*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_OBJTR.
*----------------------------------------------------------------------*

CLASS lcl_object_tree DEFINITION INHERITING FROM lcl_view_base.
  PUBLIC SECTION.
    TYPES:
      gty_class_tab TYPE STANDARD TABLE OF abap_classname.
    EVENTS:
      show_annotations_requested EXPORTING VALUE(component)       TYPE zaap_l_componentid
                                           VALUE(componenttype)   TYPE zaap_l_componenttype
                                           VALUE(parentcomponent) TYPE zaap_l_parcompid,
      update_requested,
      select_requested,
      add_annotations_requested EXPORTING VALUE(component)       TYPE zaap_l_componentid
                                          VALUE(componenttype)   TYPE zaap_l_componenttype
                                          VALUE(parentcomponent) TYPE zaap_l_parcompid
                                          VALUE(annotations)     TYPE gty_class_tab.
    METHODS:
      constructor IMPORTING io_container TYPE REF TO cl_gui_container
                  RAISING   cx_salv_error,
      update_content RAISING cx_salv_error
                             lcx_error,
      update_processor IMPORTING io_processor TYPE REF TO zcl_aap_proc_object,
      display REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      gty_object_tab TYPE STANDARD TABLE OF zaap_s_object WITH DEFAULT KEY.
    CONSTANTS:
      gc_fc_select  TYPE salv_de_function VALUE 'SELECT',
      gc_fc_refresh TYPE salv_de_function VALUE 'REFRESH'.
    CLASS-METHODS:
      add_node_to_tree IMPORTING io_tree            TYPE REF TO cl_salv_tree
                                 iv_related_node    TYPE salv_de_node_key OPTIONAL
                                 iv_objectname      TYPE seoclsname OPTIONAL
                                 iv_parentcomponent TYPE zaap_l_parcompid OPTIONAL
                                 iv_component       TYPE zaap_l_componentid OPTIONAL
                                 iv_componenttype   TYPE zaap_l_componenttype OPTIONAL
                                 iv_annotated       TYPE zaap_l_annotated OPTIONAL
                                 iv_annotationcount TYPE zaap_l_annotationcount OPTIONAL
                                 iv_text            TYPE lvc_value OPTIONAL
                                 iv_icon            TYPE icon_l2 OPTIONAL
                                 iv_annotatable     TYPE abap_bool OPTIONAL
                       RETURNING VALUE(ro_node)     TYPE REF TO cl_salv_node
                       RAISING   cx_salv_msg.
    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_tree
        IMPORTING columnname node_key,
      on_added_function FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_drop FOR EVENT on_drop OF cl_gui_alv_tree
        IMPORTING drag_drop_object node_key,
      update_drag_drop_nodes.
    DATA:
      mo_tree             TYPE REF TO cl_salv_tree,
      mt_objects          TYPE gty_object_tab,
      mo_object_processor TYPE REF TO zcl_aap_proc_object.
ENDCLASS.

CLASS lcl_object_tree IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_container ).

    cl_salv_tree=>factory(
      EXPORTING
        r_container   = io_container
      IMPORTING
        r_salv_tree   = mo_tree
      CHANGING
        t_table       = mt_objects
    ).

    mo_tree->get_functions(:
      )->set_collapse( abap_true ),
      )->set_expand( abap_true ),
      )->set_find( abap_true ),
      )->set_layout_change( abap_true ),
      )->set_print_all( abap_true ),
      )->set_print_all_preview( abap_true ),
      )->set_print_view( abap_true ),
      )->set_print_view_preview( abap_true ).

    mo_tree->get_functions( )->add_function(
      name               = gc_fc_refresh
      icon               = CONV #( icon_refresh )
      tooltip            = CONV #( 'Refresh'(002) )
      position           = if_salv_c_function_position=>right_of_salv_functions
    ).
    mo_tree->get_functions( )->add_function(
      name               = gc_fc_select
      icon               = CONV #( icon_search )
      text               = CONV #( 'Open development object'(001) )
      tooltip            = CONV #( 'Open development object'(001) )
      position           = if_salv_c_function_position=>right_of_salv_functions
    ).
*    mo_tree->get_functions( )->add_function(
*      name               = gc_fc_annotate
*      icon               = CONV #( icon_add_row )
*      text               = CONV #( 'Annotate'(003) )
*      tooltip            = CONV #( 'Annotate'(003) )
*      position           = if_salv_c_function_position=>right_of_salv_functions
*    ).
    mo_tree->get_columns( )->set_optimize( ).
    mo_tree->get_tree_settings( )->set_hierarchy_header( 'Hierarchy'(004) ).
    mo_tree->get_columns( )->get_column(:
      'OBJECTNAME' )->set_visible( abap_false ),
      'COMPONENT' )->set_visible( abap_false ),
      'COMPONENTTYPE' )->set_visible( abap_false ),
      'PARENTCOMPONENT' )->set_visible( abap_false )
    .

    SET HANDLER on_double_click FOR mo_tree->get_event( ).
    SET HANDLER on_added_function FOR mo_tree->get_event( ).
    SET HANDLER on_drop FOR lcl_salv_backdoor=>get_alv_from_salv( mo_tree ).
  ENDMETHOD.

  METHOD on_double_click.
    DATA: lr_row TYPE REF TO zaap_s_object.

    TRY.
        lr_row = CAST #( mo_tree->get_nodes( )->get_node( node_key )->get_data_row( ) ).
        RAISE EVENT show_annotations_requested
          EXPORTING
            component       = lr_row->component
            componenttype   = lr_row->componenttype
            parentcomponent = lr_row->parentcomponent.
      CATCH cx_salv_msg INTO DATA(lx_ex).
        MESSAGE lx_ex TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_salv_function.
      WHEN gc_fc_refresh.
        RAISE EVENT update_requested.
      WHEN gc_fc_select.
        RAISE EVENT select_requested.
    ENDCASE.
  ENDMETHOD.

  METHOD on_drop.
    IF drag_drop_object->object IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(lo_drop) = CAST lcl_annotation_drop( drag_drop_object->object ).
        DATA(lr_data) = CAST zaap_s_object(
                          mo_tree->get_nodes( )->get_node( node_key )->get_data_row( )
                        ).
        IF lo_drop->mt_annotations IS NOT INITIAL.
          RAISE EVENT add_annotations_requested
            EXPORTING
              component       = lr_data->component
              componenttype   = lr_data->componenttype
              parentcomponent = lr_data->parentcomponent
              annotations     = lo_drop->mt_annotations.
        ENDIF.
      CATCH cx_sy_move_cast_error cx_salv_msg ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD update_content.
    DATA(lo_nodes) = mo_tree->get_nodes( ).
    lo_nodes->delete_all( ).

    IF mo_object_processor IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lo_descr) = mo_object_processor->mo_object_descr.

    DATA(lv_object_name) = CONV seoclsname( lo_descr->get_relative_name( ) ).

    " Root element -> class / interface
    DATA(lv_root_icon) = SWITCH #( lo_descr->type_kind
                                   WHEN cl_abap_typedescr=>typekind_class
                                   THEN icon_oo_class
                                   WHEN cl_abap_typedescr=>typekind_intf
                                   THEN icon_oo_interface ).
    DATA(lo_root) = add_node_to_tree(
      io_tree            = mo_tree
      iv_objectname      = lv_object_name
      iv_annotated       = mo_object_processor->has_annotations( )
      iv_annotationcount = CONV #( mo_object_processor->get_annotation_count( ) )
      iv_text            = CONV #( lv_object_name )
      iv_icon            = lv_root_icon
      iv_annotatable     = mo_object_processor->is_annotatable( )
    ).

    " Attributes
    LOOP AT lo_descr->attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
      DATA(lo_attribute_processor)
        = mo_object_processor->get_attribute_processor( <ls_attribute>-name ).
      DATA(lv_attribute_icon) = COND #( WHEN <ls_attribute>-is_class = abap_true
                                        THEN icon_oo_class_attribute
                                        ELSE icon_oo_inst_attribute ).
      add_node_to_tree(
        io_tree            = mo_tree
        iv_related_node    = lo_root->get_key( )
        iv_objectname      = lv_object_name
        iv_component       = CONV #( <ls_attribute>-name )
        iv_componenttype   = 'A'
        iv_annotated       = lo_attribute_processor->has_annotations( )
        iv_annotationcount = CONV #( lo_attribute_processor->get_annotation_count( ) )
        iv_text            = CONV #( <ls_attribute>-name )
        iv_icon            = lv_attribute_icon
        iv_annotatable     = lo_attribute_processor->is_annotatable( )
      ).
    ENDLOOP.

    " Methods
    LOOP AT lo_descr->methods ASSIGNING FIELD-SYMBOL(<ls_method>).
      DATA(lo_method_processor) = mo_object_processor->get_method_processor( <ls_method>-name ).
      DATA(lv_method_icon) = COND #( WHEN <ls_method>-is_class = abap_true
                                     THEN icon_oo_class_method
                                     ELSE icon_oo_inst_method ).
      DATA(lo_method) = add_node_to_tree(
        io_tree            = mo_tree
        iv_related_node    = lo_root->get_key( )
        iv_objectname      = lv_object_name
        iv_component       = CONV #( <ls_method>-name )
        iv_componenttype   = 'M'
        iv_annotated       = lo_method_processor->has_annotations( )
        iv_annotationcount = CONV #( lo_method_processor->get_annotation_count( ) )
        iv_text            = CONV #( <ls_method>-name )
        iv_icon            = lv_method_icon
        iv_annotatable     = lo_method_processor->is_annotatable( )
      ).

      " Method parameters
      LOOP AT <ls_method>-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
        DATA(lo_parameter_processor)
          = lo_method_processor->get_parameter_processor( <ls_parameter>-name ).
        DATA(lv_parameter_icon) = SWITCH #( <ls_parameter>-parm_kind
                                            WHEN cl_abap_objectdescr=>importing
                                            THEN icon_parameter_import
                                            WHEN cl_abap_objectdescr=>exporting
                                            THEN icon_parameter_export
                                            WHEN cl_abap_objectdescr=>changing
                                            THEN icon_parameter_changing
                                            WHEN cl_abap_objectdescr=>returning
                                            THEN icon_parameter_result ).
        add_node_to_tree(
          io_tree            = mo_tree
          iv_related_node    = lo_method->get_key( )
          iv_objectname      = lv_object_name
          iv_parentcomponent = CONV #( <ls_method>-name )
          iv_component       = CONV #( <ls_parameter>-name )
          iv_componenttype   = 'P'
          iv_annotated       = lo_parameter_processor->has_annotations( )
          iv_annotationcount = CONV #( lo_parameter_processor->get_annotation_count( ) )
          iv_text            = CONV #( <ls_parameter>-name )
          iv_icon            = lv_parameter_icon
          iv_annotatable     = lo_parameter_processor->is_annotatable( )
        ).
      ENDLOOP.
    ENDLOOP.

    lo_nodes->expand_all( ).

    update_drag_drop_nodes( ).
  ENDMETHOD.

  METHOD add_node_to_tree.
    ro_node = io_tree->get_nodes( )->add_node(
      related_node   = iv_related_node
      relationship   = if_salv_c_node_relation=>last_child
      data_row       = VALUE zaap_s_object(
                         component       = iv_component
                         componenttype   = iv_componenttype
                         objectname      = iv_objectname
                         annotated       = iv_annotated
                         annotationcount = iv_annotationcount
                         parentcomponent = iv_parentcomponent
                         annotatable     = iv_annotatable
                       )
      collapsed_icon = CONV #( iv_icon )
      expanded_icon  = CONV #( iv_icon )
      row_style      = COND #( WHEN iv_annotated = abap_true
                               THEN if_salv_c_tree_style=>emphasized_positive
                               WHEN iv_annotatable = abap_false
                               THEN if_salv_c_tree_style=>inactive )
      text           = iv_text
    ).
  ENDMETHOD.

  METHOD display.
    mo_tree->display( ).
  ENDMETHOD.

  METHOD update_processor.
    mo_object_processor = io_processor.
  ENDMETHOD.

  METHOD update_drag_drop_nodes.
    DATA(lo_alv) = lcl_salv_backdoor=>get_alv_from_salv( mo_tree ).

    DATA(lo_dragdrop) = NEW cl_dragdrop( ).
    lo_dragdrop->add(
      flavor          = 'LCL_ANNOTATION_DROP'
      dragsrc         = abap_false
      droptarget      = abap_true
      effect          = cl_dragdrop=>move + cl_dragdrop=>copy
    ).
    lo_dragdrop->get_handle( IMPORTING handle = DATA(lv_handle) ).

    TRY.
        LOOP AT mo_tree->get_nodes( )->get_all_nodes( ) ASSIGNING FIELD-SYMBOL(<ls_node>).
          DATA(lr_data) = CAST zaap_s_object( <ls_node>-node->get_data_row( ) ).
          IF lr_data->annotatable = abap_true.
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
