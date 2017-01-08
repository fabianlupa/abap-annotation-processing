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
      "! Get all child classes for a given class
      "! @parameter iv_base_class_name | Name of the base class
      "! @parameter rt_subclasses | Found child classes
      "! @raising zcx_aap_illegal_argument | Base class does not exist
      get_subclasses_of_class IMPORTING iv_base_class_name   TYPE abap_classname
                              RETURNING VALUE(rt_subclasses) TYPE gty_class_tab
                              RAISING   zcx_aap_illegal_argument,
      "! Get package name for a class
      "! @parameter iv_class_name | Name of the class
      "! @parameter rv_devclass | Package name
      "! @raising zcx_aap_illegal_argument | iv_class_name invalid
      get_devclass_for_class IMPORTING iv_class_name      TYPE abap_classname
                             RETURNING VALUE(rv_devclass) TYPE devclass
                             RAISING   zcx_aap_illegal_argument,
      "! Get parent package for a package
      "! <p>
      "! If the child package does not exist or there is not parent package the result will be
      "! empty.
      "! </p>
      "! @parameter iv_devclass | Child devclass
      "! @parameter rv_parent_devclass | Parent devclass
      get_parent_devclass IMPORTING iv_devclass               TYPE devclass
                          RETURNING VALUE(rv_parent_devclass) TYPE devclass,
      "! Get objectdescriptor from data variable
      "! @parameter ig_data | Variable typed as REF TO interface/class
      "! @parameter ro_descriptor | Object descriptor
      get_objectdescr_from_data IMPORTING ig_data              TYPE any
                                RETURNING VALUE(ro_descriptor) TYPE REF TO cl_abap_objectdescr.
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
          iv_value  = iv_intf_name ##NO_TEXT.
    ENDIF.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      INSERT <ls_result>-clsname INTO TABLE rt_classes.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_devclass_for_class.
    CALL FUNCTION 'DEV_GET_DEVCLASS_FROM_OBJECT'
      EXPORTING
        i_pgmid    = 'R3TR'
        i_objtype  = 'CLAS'
        i_objname  = CONV sobj_name( iv_class_name )
      IMPORTING
        e_devclass = rv_devclass.

    IF rv_devclass IS INITIAL.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = zcx_aap_illegal_argument=>gc_with_name
          iv_name   = 'IV_CLASS_NAME'
          iv_value  = iv_class_name ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_subclasses_of_class.
    " Check if class exists
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_base_class_name    " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_descr)    " Reference to description object
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0 OR lo_descr IS NOT BOUND
        OR lo_descr->type_kind <> cl_abap_typedescr=>typekind_class.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = zcx_aap_illegal_argument=>gc_with_name_and_reason
          iv_name   = 'IV_BASE_CLASS_NAME'
          iv_reason = iv_base_class_name && ' is not a class'(001)
          iv_value  = iv_base_class_name ##NO_TEXT.
    ENDIF.

    SELECT clsname INTO TABLE @rt_subclasses
      FROM seometarel
      WHERE refclsname = @iv_base_class_name
        AND reltype    = '2'
        AND version    = '1'
      ORDER BY clsname.
  ENDMETHOD.

  METHOD get_parent_devclass.
    CALL FUNCTION 'DEV_GET_PARENTPACK_FROM_OBJECT'
      EXPORTING
        i_pgmid      = 'R3TR'    " Programm-ID in Aufträgen und Aufgaben
        i_objtype    = 'DEVC'    " Objekttyp
        i_objname    = CONV sobj_name( iv_devclass )    " Objektname im Objektkatalog
      IMPORTING
        e_parentpack = rv_parent_devclass.     " Paket
  ENDMETHOD.

  METHOD get_objectdescr_from_data.
    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( ig_data ).

    IF lo_descr->type_kind <> cl_abap_typedescr=>typekind_oref.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument. " TODO: Add exception message
    ENDIF.

    DATA(lo_referenced_descr) = CAST cl_abap_refdescr( lo_descr )->get_referenced_type( ).
    IF lo_referenced_descr->type_kind <> cl_abap_typedescr=>typekind_class
        AND lo_referenced_descr->type_kind <> cl_abap_typedescr=>typekind_intf.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument. " TODO: Add exception message
    ENDIF.

    ro_descriptor = CAST cl_abap_objectdescr( lo_referenced_descr ).
  ENDMETHOD.
ENDCLASS.