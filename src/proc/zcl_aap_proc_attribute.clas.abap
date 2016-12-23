CLASS zcl_aap_proc_attribute DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_proc_base
  CREATE PRIVATE.

  PUBLIC SECTION.
    METHODS:
      load_all REDEFINITION,
      is_annotation_present_by_name REDEFINITION,
      is_annotation_present_by_descr REDEFINITION,
      get_annotation_by_name REDEFINITION,
      get_annotation_by_descr REDEFINITION,
      get_annotations REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_proc_attribute IMPLEMENTATION.
  METHOD load_all.

  ENDMETHOD.

  METHOD get_annotations.

  ENDMETHOD.

  METHOD get_annotation_by_descr.

  ENDMETHOD.

  METHOD get_annotation_by_name.

  ENDMETHOD.

  METHOD is_annotation_present_by_descr.

  ENDMETHOD.

  METHOD is_annotation_present_by_name.

  ENDMETHOD.
ENDCLASS.