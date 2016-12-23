CLASS zcl_aap_proc_method DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_proc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "!
      "! @parameter ro_processor |
      get_parameter_processor RETURNING VALUE(ro_processor) TYPE REF TO zcl_aap_proc_parameter,
      load_all REDEFINITION,
      is_annotation_present_by_name REDEFINITION,
      is_annotation_present_by_descr REDEFINITION,
      get_annotation_by_name REDEFINITION,
      get_annotation_by_descr REDEFINITION,
      get_annotations REDEFINITION.
    DATA:
      mv_method_name TYPE abap_methname READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_proc_method IMPLEMENTATION.
  METHOD load_all.

  ENDMETHOD.

  METHOD get_annotations.

  ENDMETHOD.

  METHOD get_annotation_by_descr.

  ENDMETHOD.

  METHOD get_annotation_by_name.

  ENDMETHOD.

  METHOD get_parameter_processor.

  ENDMETHOD.

  METHOD is_annotation_present_by_descr.

  ENDMETHOD.

  METHOD is_annotation_present_by_name.

  ENDMETHOD.
ENDCLASS.