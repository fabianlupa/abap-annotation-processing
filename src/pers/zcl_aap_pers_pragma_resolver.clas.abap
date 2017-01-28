"! Pragma annotation resolver
CLASS zcl_aap_pers_pragma_resolver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_aap_annotation_resolver.
    ALIASES:
      get_annotations_for_attribute FOR zif_aap_annotation_resolver~get_annotations_for_attribute,
      get_annotations_for_method FOR zif_aap_annotation_resolver~get_annotations_for_method,
      get_annotations_for_object FOR zif_aap_annotation_resolver~get_annotations_for_object,
      get_annotations_for_parameter FOR zif_aap_annotation_resolver~get_annotations_for_parameter.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_pers_pragma_resolver IMPLEMENTATION.
  METHOD get_annotations_for_attribute.
    RAISE EXCEPTION TYPE zcx_aap_not_implemented.
  ENDMETHOD.

  METHOD get_annotations_for_method.
    RAISE EXCEPTION TYPE zcx_aap_not_implemented.
  ENDMETHOD.

  METHOD get_annotations_for_object.
    RAISE EXCEPTION TYPE zcx_aap_not_implemented.
  ENDMETHOD.

  METHOD get_annotations_for_parameter.
    RAISE EXCEPTION TYPE zcx_aap_not_implemented.
  ENDMETHOD.
ENDCLASS.