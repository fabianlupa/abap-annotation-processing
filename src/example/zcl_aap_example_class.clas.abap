"! Annotated class for examples
CLASS zcl_aap_example_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_aap_annotatable.
    METHODS:
      annotated_method IMPORTING iv_annotated_parameter TYPE string.
    DATA:
      mv_not_annotated_inst_attr TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AAP_EXAMPLE_CLASS IMPLEMENTATION.


  METHOD annotated_method ##NEEDED.
  ENDMETHOD.
ENDCLASS.