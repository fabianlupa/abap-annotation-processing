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
      get_annotations REDEFINITION.
    DATA:
      "! Name of the containing class or interface
      mv_containing_object_name TYPE abap_classname READ-ONLY,
      "! Attribute description
      ms_attribute_description  TYPE abap_attrdescr READ-ONLY,
      "! Attribute name in its containing object
      mv_attribute_name         TYPE abap_attrname READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING is_attribute_description  TYPE abap_attrdescr
                            iv_containing_object_name TYPE abap_classname.
ENDCLASS.



CLASS zcl_aap_proc_attribute IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    ASSERT: iv_containing_object_name IS NOT INITIAL,
            is_attribute_description IS NOT INITIAL.

    mv_containing_object_name = iv_containing_object_name.
    ms_attribute_description = is_attribute_description.
    mv_attribute_name = is_attribute_description-name.
  ENDMETHOD.

  METHOD load_all ##NEEDED.
    " Nothing to do here
  ENDMETHOD.

  METHOD get_annotations.
    " TODO: Think of propagating the exception in some way
    rt_annotations = get_resolver( )->get_annotations_for_attribute(
                         iv_containing_object_name = mv_containing_object_name
                         iv_attribute_name         = mv_attribute_name
                     ).
  ENDMETHOD.
ENDCLASS.