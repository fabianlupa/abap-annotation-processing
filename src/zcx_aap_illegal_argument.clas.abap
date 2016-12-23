"! Illegal argument exception
CLASS zcx_aap_illegal_argument DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_name,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_name,
      BEGIN OF gc_with_name_and_reason,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_NAME',
        attr2 TYPE scx_attrname VALUE 'MV_REASON',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_name_and_reason,
      BEGIN OF gc_nullpointer,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_nullpointer,
      BEGIN OF gc_nullpointer_with_name,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_nullpointer_with_name,
      BEGIN OF gc_not_in_range,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_in_range,
      BEGIN OF gc_not_in_range_with_value,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MV_VALUE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_in_range_with_value,
      BEGIN OF gc_not_in_range_with_valuename,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MV_VALUE',
        attr2 TYPE scx_attrname VALUE 'MV_NAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_in_range_with_valuename.
    INTERFACES:
      if_t100_message.
    CLASS-METHODS:
      "! Raise exception if parameter value is a null reference
      "! @parameter io_ref | Reference to check
      "! @parameter iv_name | Name of the parameter
      "! @raising zcx_aap_illegal_argument | Value is null
      raise_if_nullpointer IMPORTING io_ref  TYPE REF TO object
                                     iv_name TYPE abap_parmname OPTIONAL
                           RAISING   zcx_aap_illegal_argument.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_name | Parameter name
      "! @parameter iv_reason | Reason
      "! @parameter iv_value | Value
      constructor IMPORTING is_textid   LIKE if_t100_message=>t100key OPTIONAL
                            ix_previous LIKE previous OPTIONAL
                            iv_name     TYPE csequence OPTIONAL
                            iv_reason   TYPE csequence OPTIONAL
                            iv_value    TYPE csequence OPTIONAL.
    DATA:
      mv_name   TYPE string,
      mv_reason TYPE string,
      mv_value  TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_aap_illegal_argument IMPLEMENTATION.
  METHOD raise_if_nullpointer.
    IF io_ref IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = COND #( WHEN iv_name IS INITIAL
                              THEN zcx_aap_illegal_argument=>gc_nullpointer
                              ELSE zcx_aap_illegal_argument=>gc_nullpointer_with_name )
          iv_name   = iv_name.
    ENDIF.
  ENDMETHOD.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_name = iv_name.
    mv_reason = iv_reason.
    mv_value = iv_value.

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = gc_no_arguments.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.