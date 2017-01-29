"! Attribute processor
CLASS zcl_aap_proc_attribute DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_proc_base
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_aap_proc_object.

  PUBLIC SECTION.
    METHODS:
      load_all REDEFINITION,
      get_annotations REDEFINITION,
      is_annotatable REDEFINITION,
      get_target REDEFINITION.
    DATA:
      "! Name of the containing class or interface
      mv_containing_object_name TYPE abap_classname READ-ONLY,
      "! Attribute description
      ms_attribute_description  TYPE abap_attrdescr READ-ONLY,
      "! Attribute name in its containing object
      mv_attribute_name         TYPE abap_attrname READ-ONLY,
      "! Object processor
      mo_object_processor       TYPE REF TO zcl_aap_proc_object READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING is_attribute_description  TYPE abap_attrdescr
                            iv_containing_object_name TYPE abap_classname
                            io_object_processor       TYPE REF TO zcl_aap_proc_object.
ENDCLASS.



CLASS zcl_aap_proc_attribute IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    ASSERT: iv_containing_object_name IS NOT INITIAL,
            is_attribute_description IS NOT INITIAL,
            io_object_processor IS BOUND.

    mv_containing_object_name = iv_containing_object_name.
    ms_attribute_description = is_attribute_description.
    mv_attribute_name = is_attribute_description-name.
    mo_object_processor = io_object_processor.
  ENDMETHOD.

  METHOD load_all ##NEEDED.
    " Nothing to do here
  ENDMETHOD.

  METHOD get_annotations.
    TRY.
        rt_annotations = get_resolver( )->get_annotations_for_attribute(
                             iv_containing_object_name = mv_containing_object_name
                             iv_attribute_name         = mv_attribute_name
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
    rv_annotatable = boolc( mo_object_processor->is_annotatable( ) AND
                            ( ms_attribute_description-is_inherited = abap_false
*                              AND ms_attribute_description-is_interface = abap_false
                              AND ms_attribute_description-alias_for IS INITIAL
                             )
                           ).
  ENDMETHOD.

  METHOD get_target.
    ro_target = zcl_aap_annotation_target=>go_attribute.
  ENDMETHOD.
ENDCLASS.
