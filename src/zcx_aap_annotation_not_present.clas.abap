"! Annotation is not present
CLASS zcx_aap_annotation_not_present DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_not_present,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MV_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_present.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_name | Annotation class name
      constructor IMPORTING ix_previous LIKE previous OPTIONAL
                            iv_name     TYPE abap_classname OPTIONAL.
    DATA:
      mv_name TYPE abap_classname.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcx_aap_annotation_not_present IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).
    mv_name = iv_name.

    CLEAR me->textid.
    if_t100_message~t100key = gc_not_present.
  ENDMETHOD.
ENDCLASS.