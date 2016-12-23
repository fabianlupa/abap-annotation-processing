"! Deprecated annotation
CLASS zcl_aap_dann_deprecated DEFINITION
  PUBLIC
  INHERITING FROM zcl_aap_annotation_base
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zif_aap_annotation_resolver.

  PUBLIC SECTION.
    DATA:
      "! Since when is the class or method deprecated (version / date / release)
      mv_since   TYPE string READ-ONLY,
      "! Comment on the deprecation (reason, alternative)
      mv_comment TYPE string READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      get_bindable_attributes REDEFINITION,
      get_targets_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_dann_deprecated IMPLEMENTATION.
  METHOD get_bindable_attributes.
    rt_attributes = VALUE #( ( CONV #( 'MV_SINCE' ) ) ( CONV #( 'MV_COMMENT' ) ) ).
  ENDMETHOD.

  METHOD get_targets_internal.
    rt_targets = VALUE #(
      ( zcl_aap_annotation_target=>go_class )
      ( zcl_aap_annotation_target=>go_method )
    ).
  ENDMETHOD.
ENDCLASS.