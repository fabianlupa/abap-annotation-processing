"! Targets enumeration with targets for annotations or annotation processors
CLASS zcl_aap_annotation_target DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA:
      "! Class / interface
      go_object    TYPE REF TO zcl_aap_annotation_target READ-ONLY,
      "! Method
      go_method    TYPE REF TO zcl_aap_annotation_target READ-ONLY,
      "! Method parameter
      go_parameter TYPE REF TO zcl_aap_annotation_target READ-ONLY,
      "! Attribute
      go_attribute TYPE REF TO zcl_aap_annotation_target READ-ONLY.
    DATA:
      mv_key TYPE char01 READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING iv_key TYPE char01.
ENDCLASS.



CLASS zcl_aap_annotation_target IMPLEMENTATION.
  METHOD class_constructor.
    init: go_object    'O',
          go_method    'M',
          go_parameter 'P',
          go_attribute 'A'.
  ENDMETHOD.

  METHOD constructor.
    mv_key = iv_key.
  ENDMETHOD.
ENDCLASS.
