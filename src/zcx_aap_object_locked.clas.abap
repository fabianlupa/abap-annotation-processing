"! Lock exception
CLASS zcx_aap_object_locked DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcx_aap_error
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_locked,
        msgid TYPE symsgid VALUE 'ZAAP',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE 'MV_TABLE_NAME',
        attr2 TYPE scx_attrname VALUE 'MV_LOCKED_BY',
        attr3 TYPE scx_attrname VALUE 'MV_TABLE_KEY',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_locked.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_table_name | Database table name
      "! @parameter iv_table_key | Key of the locked entry
      "! @parameter iv_locked_by | Locked by user
      constructor IMPORTING ix_previous   LIKE previous OPTIONAL
                            iv_table_name TYPE tabname16 OPTIONAL
                            iv_table_key  TYPE csequence OPTIONAL
                            iv_locked_by  TYPE username OPTIONAL.
    DATA:
      mv_table_name TYPE tabname16 READ-ONLY,
      mv_table_key  TYPE string READ-ONLY,
      mv_locked_by  TYPE username READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_aap_object_locked IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( is_textid = gc_locked ix_previous = ix_previous ).

    mv_table_name = iv_table_name.
    mv_table_key = iv_table_key.
    mv_locked_by = iv_locked_by.
  ENDMETHOD.
ENDCLASS.
