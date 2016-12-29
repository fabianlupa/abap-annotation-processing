"! Annotation base class
CLASS zcl_aap_annotation_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED
  GLOBAL FRIENDS zif_aap_annotation_resolver.

  PUBLIC SECTION.
    CONSTANTS:
      gc_classname TYPE abap_classname VALUE 'ZCL_AAP_ANNOTATION_BASE'.
    TYPES:
      gty_target_list TYPE HASHED TABLE OF REF TO zcl_aap_annotation_target
                           WITH UNIQUE KEY table_line.
    METHODS:
      "!
      "! @parameter rt_targets |
      get_targets FINAL RETURNING VALUE(rt_targets) TYPE gty_target_list.
  PROTECTED SECTION.
    TYPES:
      gty_attribute_tab TYPE SORTED TABLE OF abap_attrname WITH UNIQUE KEY table_line.
    METHODS:
      constructor,
      "!
      "! @parameter rt_attributes |
      get_bindable_attributes RETURNING VALUE(rt_attributes) TYPE gty_attribute_tab,
      "!
      "! @parameter rt_targets |
      get_targets_internal RETURNING VALUE(rt_targets) TYPE gty_target_list.
  PRIVATE SECTION.
    DATA:
      mt_targets TYPE gty_target_list.
ENDCLASS.



CLASS zcl_aap_annotation_base IMPLEMENTATION.
  METHOD constructor.
    mt_targets = get_targets_internal( ).
  ENDMETHOD.

  METHOD get_bindable_attributes ##NEEDED.
    " Should be overwritten in subclasses, if they have bindable attributes
  ENDMETHOD.

  METHOD get_targets_internal.
    " Should be overwritten in subclasses, if they have other targets than these default ones
    rt_targets = VALUE #(
      ( zcl_aap_annotation_target=>go_attribute )
      ( zcl_aap_annotation_target=>go_class )
      ( zcl_aap_annotation_target=>go_parameter )
      ( zcl_aap_annotation_target=>go_method )
    ).
  ENDMETHOD.

  METHOD get_targets.
    rt_targets = mt_targets.
  ENDMETHOD.
ENDCLASS.