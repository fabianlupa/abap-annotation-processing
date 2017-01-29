"! AAP dynamic check exception base class
"! <p>
"! Exceptions of this type should occur if the method caller is responsible for the error because
"! of API missusage and can be expected to use it in the intended way.
"! </p>
"! <p>
"! This exception should generally <strong>not be caught</strong> in a try-catch block because it
"! indicates a programming error by the API user and thus it should create a dump in ST22 with a
"! <em>CX_SY_NO_HANDLER</em> exception.
"! </p>
CLASS zcx_aap_call_error DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_call_error,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_call_error.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING is_textid   LIKE if_t100_message=>t100key OPTIONAL
                            ix_previous LIKE previous OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_aap_call_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = gc_call_error.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
