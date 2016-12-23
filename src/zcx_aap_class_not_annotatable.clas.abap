"! Class does not implement interface ZIF_AAP_ANNOTATABLE
CLASS zcx_aap_class_not_annotatable DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcx_aap_illegal_argument
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_not_annotatable,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_annotatable.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_name | Parameter name
      "! @parameter iv_classname | Name of the class that does not implement the interface
      constructor IMPORTING ix_previous  LIKE previous OPTIONAL
                            iv_name      TYPE csequence OPTIONAL
                            iv_classname TYPE abap_classname OPTIONAL.
    DATA:
      mv_classname TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcx_aap_class_not_annotatable IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      EXPORTING
        is_textid   = gc_not_annotatable
        ix_previous = ix_previous
        iv_name     = iv_name
    ).
    mv_classname = iv_classname.
  ENDMETHOD.
ENDCLASS.