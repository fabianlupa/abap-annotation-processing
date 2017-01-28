"! Database annotation changer
CLASS zcl_aap_pers_db_changer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_aap_annotation_changer.
    ALIASES:
      add_to_object FOR zif_aap_annotation_changer~add_to_object,
      add_to_method FOR zif_aap_annotation_changer~add_to_method,
      add_to_parameter FOR zif_aap_annotation_changer~add_to_parameter,
      add_to_attribute FOR zif_aap_annotation_changer~add_to_attribute,
      remove_from_object FOR zif_aap_annotation_changer~remove_from_object,
      remove_from_method FOR zif_aap_annotation_changer~remove_from_method,
      remove_from_parameter FOR zif_aap_annotation_changer~remove_from_parameter,
      remove_from_attribute FOR zif_aap_annotation_changer~remove_from_attribute,
      change_for_object FOR zif_aap_annotation_changer~change_for_object,
      change_for_method FOR zif_aap_annotation_changer~change_for_method,
      change_for_parameter FOR zif_aap_annotation_changer~change_for_parameter,
      change_for_attribute FOR zif_aap_annotation_changer~change_for_attribute.
    METHODS:
      "! Lock an annotatable class or interface
      "! @parameter iv_objectname | Class / interfacename
      "! @raising zcx_aap_object_locked | Object is already locked by someone else
      "! @raising zcx_aap_class_not_annotatable | Object does not implement ZIF_AAP_ANNOTATABLE
      "! @raising zcx_aap_illegal_argument | iv_objectname does not resolve to a class or interface
      lock IMPORTING iv_objectname TYPE abap_classname
           RAISING   zcx_aap_object_locked
                     zcx_aap_class_not_annotatable
                     zcx_aap_illegal_argument,
      "! Lock an annotatable class or interface
      "! @parameter iv_objectname | Class / interfacename
      "! @raising zcx_aap_class_not_annotatable | Object does not implement ZIF_AAP_ANNOTATABLE
      "! @raising zcx_aap_illegal_argument | iv_objectname does not resolve to a class or interface
      unlock IMPORTING iv_objectname TYPE abap_classname
             RAISING   zcx_aap_class_not_annotatable
                       zcx_aap_illegal_argument.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      add_to_any IMPORTING is_key        TYPE zaap_tcassociat
                           it_parameters TYPE zif_aap_annotation_changer=>gty_parameter_tab OPTIONAL
                 RAISING   zcx_aap_annot_change_failure,
      remove_from_any IMPORTING is_key TYPE zaap_tcassociat
                      RAISING   zcx_aap_annot_change_failure,
      change_for_any IMPORTING is_key        TYPE zaap_tcassociat
                               it_parameters TYPE zif_aap_annotation_changer=>gty_parameter_tab
                     RAISING   zcx_aap_annot_change_failure.
ENDCLASS.



CLASS zcl_aap_pers_db_changer IMPLEMENTATION.
  METHOD add_to_any.
    " Check if line already exists
    SELECT SINGLE COUNT(*)
      FROM zaap_tcassociat
      WHERE objectname     = @is_key-objectname
        AND methname       = @is_key-methname
        AND parmname       = @is_key-parmname
        AND attrname       = @is_key-attrname
        AND annotationname = @is_key-annotationname.
    IF sy-dbcnt > 0.
      RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
        EXPORTING
          iv_reason    = 'Entry already exists'(003)
          iv_operation = zcx_aap_annot_change_failure=>gc_operations-insert.
    ENDIF.

    TRY.
        lock( is_key-objectname ).

        INSERT zaap_tcassociat FROM is_key.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
            EXPORTING
              iv_reason    = |{ 'Internal error'(001) }: { sy-subrc }|
              iv_operation = zcx_aap_annot_change_failure=>gc_operations-insert.
        ENDIF.

        IF it_parameters IS NOT INITIAL.
          change_for_any( is_key        = is_key
                          it_parameters = it_parameters ).
        ENDIF.

        unlock( is_key-objectname ).

      CATCH zcx_aap_object_locked zcx_aap_class_not_annotatable zcx_aap_illegal_argument
          INTO DATA(lx_ex).

        RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
          EXPORTING
            ix_previous  = lx_ex
            iv_reason    = lx_ex->get_text( )
            iv_operation = zcx_aap_annot_change_failure=>gc_operations-insert.
    ENDTRY.
  ENDMETHOD.


  METHOD add_to_attribute.
    add_to_any( VALUE #( objectname     = iv_objectname
                         attrname       = iv_attributename
                         annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD add_to_method.
    add_to_any( VALUE #( objectname     = iv_objectname
                         methname       = iv_methodname
                         annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD add_to_object.
    add_to_any( VALUE #( objectname     = iv_objectname
                         annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD add_to_parameter.
    add_to_any( VALUE #( objectname     = iv_objectname
                         methname       = iv_methodname
                         parmname       = iv_parametername
                         annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD change_for_any.
    DATA: lt_parameters      TYPE STANDARD TABLE OF zaap_tcassocdet,
          lt_current_entries TYPE STANDARD TABLE OF zaap_tcassocdet.

    " Check if line already exists
    SELECT SINGLE COUNT(*)
      FROM zaap_tcassociat
      WHERE objectname     = @is_key-objectname
        AND methname       = @is_key-methname
        AND parmname       = @is_key-parmname
        AND attrname       = @is_key-attrname
        AND annotationname = @is_key-annotationname.
    IF sy-dbcnt <> 0.
      RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
        EXPORTING
          iv_reason    = 'Entry does not exist'(002)
          iv_operation = zcx_aap_annot_change_failure=>gc_operations-modify.
    ENDIF.

    TRY.
        lock( is_key-objectname ).

        " Get the current entries
        SELECT objectname, methname, parmname, attrname, annotationname, annotationattr
          INTO TABLE @lt_current_entries
          FROM zaap_tcassocdet
          WHERE objectname     = @is_key-objectname
            AND methname       = @is_key-methname
            AND parmname       = @is_key-parmname
            AND attrname       = @is_key-attrname
            AND annotationname = @is_key-annotationname.

        LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
          APPEND VALUE #( ) TO lt_parameters ASSIGNING FIELD-SYMBOL(<ls_new_line>).
          <ls_new_line> = CORRESPONDING #( is_key ).
          <ls_new_line>-annotationattr = <ls_parameter>-name.
          <ls_new_line>-value = <ls_parameter>-value.

          " Remove this entry from the ones to be deleted
          DELETE lt_current_entries FROM <ls_new_line>.
        ENDLOOP.

        UNASSIGN: <ls_parameter>, <ls_new_line>.

        MODIFY zaap_tcassocdet FROM TABLE lt_parameters.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
            EXPORTING
              iv_reason    = |{ 'Internal error'(001) }: { sy-subrc }|
              iv_operation = zcx_aap_annot_change_failure=>gc_operations-modify.
        ENDIF.

        " The left over entries in lt_current_entries were not specified in it_parameters so they
        " should be removed
        DELETE zaap_tcassocdet FROM TABLE lt_current_entries.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
            EXPORTING
              iv_reason    = |{ 'Internal error'(001) }: { sy-subrc }|
              iv_operation = zcx_aap_annot_change_failure=>gc_operations-delete.
        ENDIF.

        unlock( is_key-objectname ).

      CATCH zcx_aap_object_locked zcx_aap_class_not_annotatable zcx_aap_illegal_argument
          INTO DATA(lx_ex).

        RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
          EXPORTING
            ix_previous  = lx_ex
            iv_reason    = lx_ex->get_text( )
            iv_operation = zcx_aap_annot_change_failure=>gc_operations-modify.
    ENDTRY.
  ENDMETHOD.


  METHOD change_for_attribute.
    change_for_any( is_key        = VALUE #( objectname     = iv_objectname
                                             attrname       = iv_attributename
                                             annotationname = iv_annotationname )
                    it_parameters = it_parameters ).
  ENDMETHOD.


  METHOD change_for_method.
    change_for_any( is_key        = VALUE #( objectname     = iv_objectname
                                             methname       = iv_methodname
                                             annotationname = iv_annotationname )
                    it_parameters = it_parameters ).
  ENDMETHOD.


  METHOD change_for_object.
    change_for_any( is_key        = VALUE #( objectname     = iv_objectname
                                             annotationname = iv_annotationname )
                    it_parameters = it_parameters ).
  ENDMETHOD.


  METHOD change_for_parameter.
    change_for_any( is_key        = VALUE #( objectname     = iv_objectname
                                             methname       = iv_methodname
                                             parmname       = iv_parametername
                                             annotationname = iv_annotationname )
                    it_parameters = it_parameters ).
  ENDMETHOD.


  METHOD remove_from_any.
    " Check if line already exists
    SELECT SINGLE COUNT(*)
      FROM zaap_tcassociat
      WHERE objectname     = @is_key-objectname
        AND methname       = @is_key-methname
        AND parmname       = @is_key-parmname
        AND attrname       = @is_key-attrname
        AND annotationname = @is_key-annotationname.
    IF sy-dbcnt <> 1.
      RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
        EXPORTING
          iv_reason    = 'Entry does not exist'(002)
          iv_operation = zcx_aap_annot_change_failure=>gc_operations-delete.
    ENDIF.

    TRY.
        lock( is_key-objectname ).

        " Delete from master table
        DELETE zaap_tcassociat FROM @is_key.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
            EXPORTING
              iv_reason    = |{ 'Internal error'(001) }: { sy-subrc }|
              iv_operation = zcx_aap_annot_change_failure=>gc_operations-delete.
        ENDIF.

        " Delete annotation attributes
        DELETE FROM zaap_tcassocdet
          WHERE objectname     = @is_key-objectname
            AND methname       = @is_key-methname
            AND parmname       = @is_key-parmname
            AND attrname       = @is_key-attrname
            AND annotationname = @is_key-annotationname.

        unlock( is_key-objectname ).

      CATCH zcx_aap_object_locked zcx_aap_class_not_annotatable zcx_aap_illegal_argument
          INTO DATA(lx_ex).

        RAISE EXCEPTION TYPE zcx_aap_annot_change_failure
          EXPORTING
            ix_previous  = lx_ex
            iv_reason    = lx_ex->get_text( )
            iv_operation = zcx_aap_annot_change_failure=>gc_operations-delete.
    ENDTRY.
  ENDMETHOD.


  METHOD remove_from_attribute.
    remove_from_any( VALUE #( objectname     = iv_objectname
                              attrname       = iv_attributename
                              annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD remove_from_method.
    remove_from_any( VALUE #( objectname     = iv_objectname
                              methname       = iv_methodname
                              annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD remove_from_object.
    remove_from_any( VALUE #( objectname     = iv_objectname
                              annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD remove_from_parameter.
    remove_from_any( VALUE #( objectname     = iv_objectname
                              methname       = iv_methodname
                              parmname       = iv_parametername
                              annotationname = iv_annotationname ) ).
  ENDMETHOD.


  METHOD lock.
    DATA: lt_locks TYPE STANDARD TABLE OF seqg3,
          li_dummy TYPE REF TO zif_aap_annotatable,
          BEGIN OF ls_key,
            clsname    TYPE seoclsname,
            refclsname TYPE seoclsname,
            version    TYPE seoversion,
          END OF ls_key.

    " Raise ZCX_AAP_CLASS_NOT_ANNOTATABLE or ZCX_AAP_ILLEGAL_ARGUMENT if necessary
    zcl_aap_proc_object=>from_name( iv_objectname ).

    DATA(lv_name) = zcl_aap_tools=>get_objectdescr_from_data( li_dummy )->get_relative_name( ).

    CALL FUNCTION 'ENQUEUE_EZAAP_ASSOC'
      EXPORTING
        clsname        = iv_objectname
        refclsname     = CONV seoclsname( lv_name )
        version        = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 1.
      " If it's already locked by someone else, find out by who
      ls_key = VALUE #( clsname = iv_objectname refclsname = lv_name version = '1' ).
      CALL FUNCTION 'ENQUEUE_READ'
        EXPORTING
          gname                 = 'SEOMETAREL'
          garg                  = CONV eqegraarg( ls_key )
          guname                = space
        TABLES
          enq                   = lt_locks
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        zcx_aap_system_error=>raise_from_sy( ).
      ENDIF.

      RAISE EXCEPTION TYPE zcx_aap_object_locked
        EXPORTING
          iv_table_name = 'SEOMETAREL'
          iv_table_key  = CONV eqegraarg( ls_key )
          iv_locked_by  = COND #( WHEN lines( lt_locks ) > 0 THEN lt_locks[ 1 ]-guname ).

    ELSEIF sy-subrc <> 0.
      zcx_aap_system_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD unlock.
    DATA: li_dummy TYPE REF TO zif_aap_annotatable.

    " Raise ZCX_AAP_CLASS_NOT_ANNOTATABLE or ZCX_AAP_ILLEGAL_ARGUMENT if necessary
    zcl_aap_proc_object=>from_name( iv_objectname ).

    DATA(lv_name) = zcl_aap_tools=>get_objectdescr_from_data( li_dummy )->get_relative_name( ).

    CALL FUNCTION 'DEQUEUE_EZAAP_ASSOC'
      EXPORTING
        clsname    = iv_objectname
        refclsname = CONV seoclsname( lv_name )
        version    = '1'.
  ENDMETHOD.
ENDCLASS.