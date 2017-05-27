*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_MSCRE.
*----------------------------------------------------------------------*

CLASS lcl_main_screen DEFINITION
  INHERITING FROM cl_bus_abstract_main_screen
  FINAL.

  PUBLIC SECTION.
    TYPES:
      gty_object_processor TYPE REF TO zcl_aap_proc_object,
      gty_obj_proc_binding TYPE REF TO gty_object_processor.
    METHODS:
      constructor IMPORTING iv_program_name  TYPE bus_screen-program_name
                            iv_dynpro_number TYPE bus_screen-dynpro_number,
      pbo_begin REDEFINITION,
      pbo_end REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      call_screen REDEFINITION,
      call_screen_starting_at REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_annotation,
        annoclass TYPE zaap_l_annoclass,
        devclass  TYPE devclass,
      END OF gty_annotation,
      gty_annotation_tab TYPE STANDARD TABLE OF gty_annotation.
    CONSTANTS:
      gc_container_name TYPE fieldname VALUE 'CC_CONTAINER',
      gc_fc_ok          TYPE busfcode VALUE 'OK',
      gc_fc_save        TYPE busfcode VALUE 'SAVE',
      gc_fc_back        TYPE busfcode VALUE 'BACK',
      gc_fc_cancel      TYPE busfcode VALUE 'CANCEL',
      gc_fc_exit        TYPE busfcode VALUE 'EXIT',
      gc_fc_zaap_check  TYPE busfcode VALUE 'ZAAP_CHECK',
      gc_fc_switch_mode TYPE busfcode VALUE 'DISP_CHANG',
      gc_status_0100    TYPE sypfkey VALUE 'STATUS_0100'.
    METHODS:
      handle_pai FOR EVENT process_after_input OF cl_bus_abstract_main_screen
        IMPORTING iv_function_code,
      update_selected_class RAISING cx_salv_error lcx_error,
      update_annotation_tree RAISING cx_salv_error,
      on_show_annotations_requested FOR EVENT show_annotations_requested OF lcl_object_tree
        IMPORTING component componenttype parentcomponent,
      on_select_requested FOR EVENT select_requested OF lcl_object_tree,
      on_update_object_requested FOR EVENT update_requested OF lcl_object_tree,
      on_add_annotations_requested FOR EVENT add_annotations_requested OF lcl_object_tree
        IMPORTING annotations component componenttype parentcomponent,
      on_annotation_double_click FOR EVENT annotation_double_click OF lcl_annotation_tree
        IMPORTING classname,
      on_annotation_update_requested FOR EVENT update_requested OF lcl_annotation_tree,
      on_add_annotation_requested FOR EVENT add_annotation_requested OF lcl_assoc_annotations_alv
        IMPORTING annotation component componenttype parentcomponent sender,
      switch_mode RAISING zcx_aap_object_locked.
    DATA:
      mo_container             TYPE REF TO cl_gui_custom_container,
      mo_annotation_tree       TYPE REF TO lcl_annotation_tree,
      mo_object_tree           TYPE REF TO lcl_object_tree,
      mo_assoc_annotations_alv TYPE REF TO lcl_assoc_annotations_alv,
      mo_splitter              TYPE REF TO cl_gui_splitter_container,
      mo_sub_splitter          TYPE REF TO cl_gui_splitter_container,
      mv_selected_class        TYPE abap_classname,
      mv_last_selected_class   TYPE abap_classname,
      mt_object_annotations    TYPE STANDARD TABLE OF zaap_s_as_annotation,
      mt_available_annotations TYPE gty_annotation_tab,
      mo_object_processor      TYPE REF TO zcl_aap_proc_object,
      mv_mode                  TYPE lcl_const=>gty_mode VALUE lcl_const=>gc_mode_view,
      mi_annotation_changer    TYPE REF TO zif_aap_annotation_changer.
ENDCLASS.

CLASS lcl_main_screen IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_program_name  = iv_program_name
                        iv_dynpro_number = iv_dynpro_number ).
    set_title( sy-title ).
    set_status( iv_status_key = gc_status_0100 iv_status_program = sy-repid ).
    SET HANDLER handle_pai FOR me.

    mi_annotation_changer = zcl_aap_dependency_injector=>get_changer( ).

    GET PARAMETER ID 'CLASS' FIELD mv_selected_class.
    IF mv_selected_class IS INITIAL.
      mv_selected_class = 'ZCL_AAP_EXAMPLE_CLASS'.
    ENDIF.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN iv_dynpro_number.
  ENDMETHOD.

  METHOD call_screen_starting_at.
    CALL SCREEN iv_dynpro_number
         STARTING AT iv_xstart iv_ystart
         ENDING AT iv_xend iv_yend.
  ENDMETHOD.

  METHOD handle_pai.
    CASE iv_function_code.
      WHEN gc_fc_ok.

      WHEN gc_fc_back OR gc_fc_cancel.
        leave( ).

      WHEN gc_fc_exit.
        LEAVE PROGRAM.

      WHEN gc_fc_zaap_check.
        CALL TRANSACTION 'ZAAP_CHECK' WITH AUTHORITY-CHECK.

      WHEN gc_fc_switch_mode.
        TRY.
            switch_mode( ).
          CATCH zcx_aap_object_locked INTO DATA(lx_ex).
            MESSAGE lx_ex TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD pbo_begin.
    set_title( |{ 'Configure annotation associations'(008) } - | &&
               |{ COND string( WHEN mv_mode = lcl_const=>gc_mode_edit THEN 'Edit'(009)
                                                                      ELSE 'View'(010) ) }| ).

    super->pbo_begin( ).

    IF mo_container IS NOT BOUND.
      mo_container = NEW cl_gui_custom_container( gc_container_name ).
      mo_splitter = NEW cl_gui_splitter_container(
                      parent     = mo_container
                      columns    = 2
                      rows       = 1
                      shellstyle = cl_gui_container=>ws_visible + cl_gui_container=>ws_child
                    ).
      mo_sub_splitter = NEW cl_gui_splitter_container(
                          parent     = mo_splitter->get_container( row = 1 column = 2 )
                          rows       = 2
                          columns    = 1
                          shellstyle = cl_gui_container=>ws_visible + cl_gui_container=>ws_child
                        ).
      TRY.
          mo_annotation_tree = NEW lcl_annotation_tree(
                                 mo_splitter->get_container( row = 1 column = 1 )
                               ).
          SET HANDLER on_annotation_double_click FOR mo_annotation_tree.
          SET HANDLER on_annotation_update_requested FOR mo_annotation_tree.

          mo_object_tree = NEW lcl_object_tree(
                             mo_sub_splitter->get_container( row = 1 column = 1 )
                           ).
          SET HANDLER on_show_annotations_requested FOR mo_object_tree.
          SET HANDLER on_select_requested FOR mo_object_tree.
          SET HANDLER on_update_object_requested FOR mo_object_tree.
          SET HANDLER on_add_annotations_requested FOR mo_object_tree.

          mo_assoc_annotations_alv = NEW lcl_assoc_annotations_alv(
                                       mo_sub_splitter->get_container( row = 2 column = 1 )
                                     ).
          SET HANDLER on_add_annotation_requested FOR mo_assoc_annotations_alv.

          update_annotation_tree( ).

        CATCH cx_salv_error INTO DATA(lx_ex).
          MESSAGE lx_ex TYPE 'E'.
      ENDTRY.

      mo_splitter->set_column_width( id = 1 width = 30 ).
      mo_sub_splitter->set_row_height( id = 2 height = 30 ).
    ENDIF.

    " Update object tree and associated annotation alv only if a new class is selected.
    " This is always the case when starting the program.
    IF mv_selected_class <> mv_last_selected_class.
      TRY.
          update_selected_class( ).
          mv_last_selected_class = mv_selected_class.
        CATCH cx_salv_error INTO DATA(lx_salv_ex).
          MESSAGE lx_salv_ex TYPE 'E'.
        CATCH lcx_error INTO DATA(lx_error).
          MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

      mo_annotation_tree->display( ).
      mo_object_tree->display( ).
      mo_assoc_annotations_alv->display( ).
    ENDIF.
  ENDMETHOD.

  METHOD pbo_end.
    DATA: lo_message TYPE REF TO cl_bus_message.
    super->pbo_end( ).

    cl_bus_message=>get_most_severe_message( EXPORTING it_messages = gt_messages
                                             IMPORTING ev_message = lo_message ).

    IF lo_message IS BOUND.
      MESSAGE ID lo_message->gs_message-msgid TYPE 'S' NUMBER lo_message->gs_message-msgno
              WITH lo_message->gs_message-msgv1
                   lo_message->gs_message-msgv2
                   lo_message->gs_message-msgv3
                   lo_message->gs_message-msgv4
              DISPLAY LIKE lo_message->gs_message-msgty.

      clear_messages( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_show_annotations_requested.
    DATA: lr_row         TYPE REF TO zaap_s_object,
          lo_processor   TYPE REF TO zcl_aap_proc_base,
          lt_annotations TYPE zif_aap_annotation_resolver=>gty_annotation_tab.

    mo_assoc_annotations_alv->update_content(
      iv_component       = component
      iv_componenttype   = componenttype
      iv_parentcomponent = parentcomponent
    ).
  ENDMETHOD.

  METHOD on_annotation_double_click.
    " Call SE80 / Eclipse router to show the class
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = classname
        object_type         = 'CLAS'
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD on_annotation_update_requested.
    TRY.
        update_annotation_tree( ).
      CATCH cx_salv_error INTO DATA(lx_ex).
        MESSAGE lx_ex TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD update_annotation_tree.
    DATA: lo_annotation_dummy TYPE REF TO zcl_aap_annotation_base.

    mo_annotation_tree->update_content(
      zcl_aap_tools=>get_subclasses_of_class(
        CONV #( zcl_aap_tools=>get_objectdescr_from_data( lo_annotation_dummy
                  )->get_relative_name( ) )
      )
    ).
  ENDMETHOD.

  METHOD update_selected_class.
    TRY.
        cl_abap_typedescr=>describe_by_name(
          EXPORTING
            p_name      = mv_selected_class
          RECEIVING
            p_descr_ref = DATA(lo_typedescr)
          EXCEPTIONS
            OTHERS      = 0
        ).
        DATA(lo_descr) = CAST cl_abap_objectdescr( lo_typedescr ).

      CATCH cx_sy_move_cast_error cx_sy_ref_is_initial INTO DATA(lx_ex).
        lcx_error=>raise(
          iv_text     = mv_selected_class && ' is not an active class or interface.'(005)
          ix_previous = lx_ex
        ).
    ENDTRY.

    mo_object_processor = zcl_aap_proc_object=>from_descriptor(
                            io_descr                = lo_descr
                            iv_skip_relevance_check = abap_true
                          ).
    mo_object_processor->load_all( ).

    " Update UI
    mo_object_tree->update_processor( mo_object_processor ).
    mo_object_tree->update_content( ).
    mo_assoc_annotations_alv->update_processor( mo_object_processor ).
    mo_assoc_annotations_alv->update_content( ).
  ENDMETHOD.

  METHOD on_select_requested.
    DATA: lv_class TYPE abap_classname.

    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type          = 'CLAS'
        suppress_selection   = abap_false
        multiple_selection   = abap_false
        use_alv_grid         = abap_true
      IMPORTING
        object_name_selected = lv_class
      EXCEPTIONS
        cancel               = 1
        wrong_type           = 2
        OTHERS               = 3.
    IF sy-subrc = 0 AND lv_class IS NOT INITIAL.
      IF mv_mode = lcl_const=>gc_mode_edit.
        TRY.
            switch_mode( ).
          CATCH zcx_aap_object_locked ##NO_HANDLER.
            " Unlocking an object does not result in this error
        ENDTRY.
      ENDIF.
      mv_selected_class = lv_class.
      SET PARAMETER ID 'CLASS' FIELD mv_selected_class.
    ENDIF.
  ENDMETHOD.

  METHOD on_update_object_requested.
    TRY.
        update_selected_class( ).
      CATCH lcx_error INTO DATA(lx_ex).
        MESSAGE lx_ex TYPE 'S' DISPLAY LIKE 'E'.
      CATCH cx_salv_error INTO DATA(lx_salv_ex).
        MESSAGE lx_salv_ex TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD on_add_annotations_requested.
    CONCATENATE LINES OF annotations INTO DATA(lv_annotations) SEPARATED BY ', '.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'TEST'
        txt1  = lv_annotations
        txt2  = space.

    TRY.
        CASE componenttype.
          WHEN space.
            LOOP AT annotations ASSIGNING FIELD-SYMBOL(<lv_annotation>).
              mi_annotation_changer->add_to_object(
                iv_objectname     = mv_selected_class
                iv_annotationname = <lv_annotation>
              ).
            ENDLOOP.
          WHEN 'A'.

          WHEN 'P'.

        ENDCASE.
      CATCH zcx_aap_annot_change_failure INTO DATA(lx_ex).
        MESSAGE lx_ex TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    mo_assoc_annotations_alv->update_content( ).
  ENDMETHOD.

  METHOD on_add_annotation_requested.
    on_add_annotations_requested(
        annotations     = VALUE #( ( annotation ) )
        component       = component
        componenttype   = componenttype
        parentcomponent = parentcomponent
    ).
  ENDMETHOD.

  METHOD switch_mode.
    DATA: lo_db_changer TYPE REF TO zcl_aap_pers_db_changer.

    " Use locks if the changer is using the database
    DATA(lo_descr) = CAST cl_abap_classdescr(
                       CAST cl_abap_refdescr(
                         cl_abap_typedescr=>describe_by_data( lo_db_changer )
                       )->get_referenced_type( )
                     ).
    IF lo_descr->applies_to( mi_annotation_changer ).
      lo_db_changer ?= mi_annotation_changer.
    ENDIF.

    IF mv_mode = lcl_const=>gc_mode_edit.
      IF lo_db_changer IS BOUND.
        lo_db_changer->unlock( mv_selected_class ).
      ENDIF.
      mv_mode = lcl_const=>gc_mode_view.
    ELSE.
      IF lo_db_changer IS BOUND.
        lo_db_changer->lock( mv_selected_class ).
      ENDIF.
      mv_mode = lcl_const=>gc_mode_edit.
    ENDIF.

    mo_assoc_annotations_alv->set_mode( mv_mode ).
  ENDMETHOD.
ENDCLASS.
