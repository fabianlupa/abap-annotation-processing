"! Failure in annotation association modification
CLASS zcx_aap_annot_change_failure DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcx_aap_error
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gty_operation TYPE char6.
    CONSTANTS:
      BEGIN OF gc_locked,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'MV_REASON',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_locked,
      BEGIN OF gc_operations,
        insert TYPE gty_operation VALUE 'INSERT',
        modify TYPE gty_operation VALUE 'MODIFY',
        delete TYPE gty_operation VALUE 'DELETE',
      END OF gc_operations.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_reason | Reason
      constructor IMPORTING ix_previous  LIKE previous OPTIONAL
                            iv_reason    TYPE csequence OPTIONAL
                            iv_operation TYPE gty_operation OPTIONAL.
    DATA:
      mv_reason    TYPE string READ-ONLY,
      mv_operation TYPE gty_operation READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_aap_annot_change_failure IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( is_textid = gc_locked ix_previous = ix_previous ).

    mv_reason = iv_reason.
    mv_operation = iv_operation.
  ENDMETHOD.
ENDCLASS.
