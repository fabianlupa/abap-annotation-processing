"! Example annotation with one attribute
CLASS zcl_aap_example_annotation DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_annotation_base
  CREATE PUBLIC
  GLOBAL FRIENDS zif_aap_annotation_resolver.

  PUBLIC SECTION.
    DATA:
      mv_attribute TYPE string VALUE `Initial value` READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      get_bindable_attributes REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_example_annotation IMPLEMENTATION.
  METHOD get_bindable_attributes.
    rt_attributes = VALUE #( ( CONV #( 'MV_ATTRIBUTE' ) ) ).
  ENDMETHOD.
ENDCLASS.