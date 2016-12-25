"! Static help methods
CLASS zcl_aap_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      gty_class_tab TYPE SORTED TABLE OF abap_classname WITH UNIQUE KEY table_line.
    CLASS-METHODS:
      "! Get all (global) classes implementing the given interface
      "! @parameter iv_intf_name | Interface name
      "! @parameter rt_classes | Implementing classes
      "! @raising zcx_aap_illegal_argument | iv_intf_name is invalid
      get_classes_implementing_intf IMPORTING iv_intf_name      TYPE abap_classname
                                    RETURNING VALUE(rt_classes) TYPE gty_class_tab
                                    RAISING   zcx_aap_illegal_argument,
      "! Get package name for a class
      "! @parameter iv_class_name | Name of the class
      "! @parameter rv_devclass | Package name
      "! @raising zcx_aap_illegal_argument | iv_class_name invalid
      get_devclass_for_class IMPORTING iv_class_name      TYPE abap_classname
                             RETURNING VALUE(rv_devclass) TYPE devclass
                             RAISING   zcx_aap_illegal_argument.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_tools IMPLEMENTATION.
  METHOD get_classes_implementing_intf.
    DATA: lt_result TYPE seor_implementing_keys.
    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = VALUE seoclskey( clsname = iv_intf_name )    " Key structure of a class
      IMPORTING
        impkeys      = lt_result
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO DATA(lv_message).
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = zcx_aap_illegal_argument=>gc_with_name_and_reason
          iv_name   = 'IV_INTF_NAME'
          iv_reason = lv_message
          iv_value  = iv_intf_name.
    ENDIF.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      INSERT <ls_result>-clsname INTO TABLE rt_classes.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_devclass_for_class.
    CALL FUNCTION 'DEV_GET_DEVCLASS_FROM_NAME'
      EXPORTING
        i_objname  = iv_class_name    " Objektname im Objektkatalog
      IMPORTING
        e_devclass = rv_devclass.    " Paket

    IF rv_devclass IS INITIAL.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = zcx_aap_illegal_argument=>gc_with_name
          iv_name   = 'IV_CLASS_NAME'
          iv_value  = iv_class_name.
    ENDIF.
  ENDMETHOD.
ENDCLASS.