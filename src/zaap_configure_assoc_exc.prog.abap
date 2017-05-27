*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_EXC.
*----------------------------------------------------------------------*

CLASS lcx_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise IMPORTING iv_text     TYPE csequence OPTIONAL
                      ix_previous TYPE REF TO cx_root OPTIONAL
                        PREFERRED PARAMETER iv_text
            RAISING   lcx_error.
    METHODS:
      constructor IMPORTING iv_text     TYPE csequence OPTIONAL
                            ix_previous TYPE REF TO cx_root OPTIONAL
                              PREFERRED PARAMETER iv_text,
      get_text REDEFINITION.
    DATA:
      mv_text TYPE string READ-ONLY.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD raise.
    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        iv_text     = iv_text
        ix_previous = ix_previous.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( previous = ix_previous ).
    mv_text = iv_text.
  ENDMETHOD.

  METHOD get_text.
    result = mv_text.
  ENDMETHOD.
ENDCLASS.
