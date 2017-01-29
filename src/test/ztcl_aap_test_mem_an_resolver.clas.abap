"! Resolve annotations from memory for testing
CLASS ztcl_aap_test_mem_an_resolver DEFINITION
  PUBLIC
  FINAL
  FOR TESTING
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_aap_annotation_resolver.
    ALIASES:
      get_annotations_for_attribute FOR zif_aap_annotation_resolver~get_annotations_for_attribute,
      get_annotations_for_method FOR zif_aap_annotation_resolver~get_annotations_for_method,
      get_annotations_for_object FOR zif_aap_annotation_resolver~get_annotations_for_object,
      get_annotations_for_parameter FOR zif_aap_annotation_resolver~get_annotations_for_parameter.
    METHODS:
      set_source CHANGING ct_source TYPE ANY TABLE.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ztcl_aap_test_mem_an_resolver IMPLEMENTATION.
  METHOD get_annotations_for_attribute.

  ENDMETHOD.

  METHOD get_annotations_for_method.

  ENDMETHOD.

  METHOD get_annotations_for_object.

  ENDMETHOD.

  METHOD get_annotations_for_parameter.

  ENDMETHOD.

  METHOD set_source.

  ENDMETHOD.
ENDCLASS.
