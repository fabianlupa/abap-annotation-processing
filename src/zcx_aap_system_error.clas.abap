"! AAP no check exception base class
"! <p>
"! Exceptions of this type indicate one of the following scenarios:
"! </p>
"! <ul>
"!   <li>Internal error in the library</li>
"!   <li>Inconsistent customizing state which prohibits further processing</li>
"! </ul>
"! <p>
"! These exceptions should either <strong>not be caught / handled at all or handled at a high level
"! in the callstack</strong> (they will not be wrapped into <em>CX_SY_NO_HANDLER</em> because of
"! <em>CX_NO_CHECK</em>-base). To prevent these exceptions tooling should be used to check integrity
"! before releasing transports to productive systems.
"! </p>
CLASS zcx_aap_system_error DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_system_error,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_system_error,
      BEGIN OF gc_with_text,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_text.
    INTERFACES:
      if_t100_message.
    CLASS-METHODS:
      "! Raise this exception using sy-msg... fields
      raise_from_sy.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_text | Error text
      constructor IMPORTING is_textid   LIKE if_t100_message=>t100key OPTIONAL
                            ix_previous LIKE previous OPTIONAL
                            iv_text     TYPE csequence OPTIONAL.
    DATA:
      mv_text TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_aap_system_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_text = iv_text.

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = gc_system_error.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_from_sy.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO DATA(lv_msg_text).

    RAISE EXCEPTION TYPE zcx_aap_system_error
      EXPORTING
        is_textid = zcx_aap_system_error=>gc_with_text
        iv_text   = lv_msg_text.
  ENDMETHOD.
ENDCLASS.
