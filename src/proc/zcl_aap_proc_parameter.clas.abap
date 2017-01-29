"! Parameter processor
CLASS zcl_aap_proc_parameter DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_proc_base
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_aap_proc_method.

  PUBLIC SECTION.
    METHODS:
      load_all REDEFINITION,
      get_annotations REDEFINITION,
      is_annotatable REDEFINITION,
      get_target REDEFINITION.
    DATA:
      "! Parameter description
      ms_parameter_description  TYPE abap_parmdescr READ-ONLY,
      "! Name of the containing class or interface
      mv_containing_object_name TYPE abap_classname READ-ONLY,
      "! Name of the containing method
      mv_containing_method_name TYPE abap_methname READ-ONLY,
      "! Name of the parameter in its containing method
      mv_parameter_name         TYPE abap_parmname READ-ONLY,
      "! Method processor
      mo_method_processor       TYPE REF TO zcl_aap_proc_method READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING is_parameter_description  TYPE abap_parmdescr
                            iv_containing_object_name TYPE abap_classname
                            iv_containing_method_name TYPE abap_methname
                            io_method_processor       TYPE REF TO zcl_aap_proc_method.
ENDCLASS.



CLASS zcl_aap_proc_parameter IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    ASSERT: is_parameter_description IS NOT INITIAL,
            iv_containing_method_name IS NOT INITIAL,
            iv_containing_object_name IS NOT INITIAL,
            io_method_processor IS BOUND.

    ms_parameter_description = is_parameter_description.
    mv_containing_method_name = iv_containing_method_name.
    mv_containing_object_name = iv_containing_object_name.
    mv_parameter_name = is_parameter_description-name.
    mo_method_processor = io_method_processor.
  ENDMETHOD.

  METHOD load_all ##NEEDED.
    " Nothing to load here
  ENDMETHOD.

  METHOD get_annotations.
    TRY.
        rt_annotations = get_resolver( )->get_annotations_for_parameter(
                             iv_containing_object_name = mv_containing_object_name
                             iv_containing_method_name = mv_containing_method_name
                             iv_parameter_name         = mv_parameter_name
                         ).
      CATCH zcx_aap_incons_customizing INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_aap_system_error
          EXPORTING
            is_textid   = zcx_aap_system_error=>gc_with_text
            ix_previous = lx_ex
            iv_text     = lx_ex->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_annotatable.
    rv_annotatable = mo_method_processor->is_annotatable( ).
  ENDMETHOD.

  METHOD get_target.
    ro_target = zcl_aap_annotation_target=>go_parameter.
  ENDMETHOD.
ENDCLASS.
