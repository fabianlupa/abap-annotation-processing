"! Method processor
CLASS zcl_aap_proc_method DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_proc_base
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_aap_proc_object.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_parameter_proc,
        parameter_name TYPE abap_parmname,
        processor      TYPE REF TO zcl_aap_proc_parameter,
      END OF gty_parameter_proc,
      gty_parameter_proc_tab TYPE SORTED TABLE OF gty_parameter_proc
                                  WITH UNIQUE KEY parameter_name.
    METHODS:
      "! Get all parameter processors
      "! @parameter rt_processors | Parameter processors
      get_parameter_processors RETURNING VALUE(rt_processors) TYPE gty_parameter_proc_tab,
      "! Get parameter processor by name
      "! @parameter iv_parameter_name | Parameter name
      "! @parameter ro_processor | Found parameter processor
      "! @raising zcx_aap_illegal_argument | Parameter does not exist
      get_parameter_processor IMPORTING iv_parameter_name   TYPE abap_parmname
                              RETURNING VALUE(ro_processor) TYPE REF TO zcl_aap_proc_parameter
                              RAISING   zcx_aap_illegal_argument,
      load_all REDEFINITION,
      get_annotations REDEFINITION,
      is_annotatable REDEFINITION,
      get_target REDEFINITION.
    DATA:
      "! Name of the containing class or interface
      mv_containing_object_name TYPE abap_classname READ-ONLY,
      "! Name of the method in its containing object
      mv_method_name            TYPE abap_methname READ-ONLY,
      "! Method description
      ms_method_description     TYPE abap_methdescr READ-ONLY,
      "! Object processor
      mo_object_processor       TYPE REF TO zcl_aap_proc_object READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING is_methdescr              TYPE abap_methdescr
                            iv_containing_object_name TYPE abap_classname
                            io_object_processor       TYPE REF TO zcl_aap_proc_object,
      load_parameters.
    DATA:
      mt_parameter_processor_cache TYPE gty_parameter_proc_tab.
ENDCLASS.



CLASS zcl_aap_proc_method IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    ASSERT: is_methdescr IS NOT INITIAL,
            iv_containing_object_name IS NOT INITIAL,
            io_object_processor IS BOUND.

    ms_method_description = is_methdescr.
    mv_method_name = is_methdescr-name.
    mv_containing_object_name = iv_containing_object_name.
    mo_object_processor = io_object_processor.
  ENDMETHOD.

  METHOD load_all.

  ENDMETHOD.

  METHOD get_annotations.
    TRY.
        rt_annotations = get_resolver( )->get_annotations_for_method(
                             iv_containing_object_name = mv_containing_object_name
                             iv_method_name            = mv_method_name
                         ).
      CATCH zcx_aap_incons_customizing INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_aap_system_error
          EXPORTING
            is_textid   = zcx_aap_system_error=>gc_with_text
            ix_previous = lx_ex
            iv_text     = lx_ex->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_parameter_processors.
    " On demand lazy loading of processors
    IF lines( mt_parameter_processor_cache ) = 0.
      load_parameters( ).
    ENDIF.

    rt_processors = mt_parameter_processor_cache.
  ENDMETHOD.

  METHOD get_parameter_processor.
    DATA(lt_processors) = get_parameter_processors( ).

    TRY.
        ro_processor = lt_processors[ parameter_name = iv_parameter_name ]-processor.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        MESSAGE e019(zaap) WITH iv_parameter_name INTO DATA(lv_reason).
        RAISE EXCEPTION TYPE zcx_aap_illegal_argument
          EXPORTING
            is_textid   = zcx_aap_illegal_argument=>gc_with_name_and_reason
            ix_previous = lx_ex
            iv_name     = 'IV_PARAMETER_NAME'
            iv_reason   = lv_reason
            iv_value    = iv_parameter_name ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD load_parameters.
    LOOP AT ms_method_description-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      INSERT VALUE #( parameter_name = <ls_parameter>-name
                      processor      = NEW zcl_aap_proc_parameter(
                                           is_parameter_description = <ls_parameter>
                                           iv_containing_object_name = mv_containing_object_name
                                           iv_containing_method_name = mv_method_name
                                           io_method_processor       = me
                                       )
                      ) INTO TABLE mt_parameter_processor_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_annotatable.
    rv_annotatable = boolc( mo_object_processor->is_annotatable( ) AND
                            ( ms_method_description-is_inherited = abap_false
                              OR ( ms_method_description-is_inherited = abap_true
                                   AND ms_method_description-is_redefined = abap_true )
                            )
                           ).
  ENDMETHOD.

  METHOD get_target.
    ro_target = zcl_aap_annotation_target=>go_method.
  ENDMETHOD.
ENDCLASS.
