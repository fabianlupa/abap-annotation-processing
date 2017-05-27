*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_EXCEP.
*----------------------------------------------------------------------*

CLASS lcx_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      raise IMPORTING iv_text     TYPE csequence OPTIONAL
                      ix_previous TYPE REF TO cx_root OPTIONAL
                        PREFERRED PARAMETER iv_text
            RAISING   lcx_error,
      raise_from_sy RAISING lcx_error.
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

  METHOD raise_from_sy.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO DATA(lv_msg).
    raise( lv_msg ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( previous = ix_previous ).
    mv_text = iv_text.
  ENDMETHOD.

  METHOD get_text.
    result = mv_text.
  ENDMETHOD.
ENDCLASS.
