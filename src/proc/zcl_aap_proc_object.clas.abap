"! Object processor
"! <p>
"! Entry point to get annotations processor intances. Use <em>from_...</em>-factory-methods to
"! obtain an instance.
"! </p>
CLASS zcl_aap_proc_object DEFINITION
  PUBLIC
  INHERITING FROM zcl_aap_proc_base
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_method_proc,
        method_name TYPE abap_methname,
        processor   TYPE REF TO zcl_aap_proc_method,
      END OF gty_method_proc,
      gty_method_proc_tab TYPE SORTED TABLE OF gty_method_proc
                               WITH UNIQUE KEY method_name,
      BEGIN OF gty_attribute_proc,
        attribute_name TYPE abap_attrname,
        processor      TYPE REF TO zcl_aap_proc_attribute,
      END OF gty_attribute_proc,
      gty_attribute_proc_tab TYPE SORTED TABLE OF gty_attribute_proc
                                  WITH UNIQUE KEY attribute_name.
    CLASS-METHODS:
      class_constructor,
      "! Create from object reference
      "! @parameter ii_annotatable | Object implementing ZIF_AAP_ANNOTATABLE
      "! @parameter ro_processor | Processor instance
      "! @raising zcx_aap_illegal_argument | ii_annotatable cannot be null
      from_object IMPORTING ii_annotatable      TYPE REF TO zif_aap_annotatable
                  RETURNING VALUE(ro_processor) TYPE REF TO zcl_aap_proc_object
                  RAISING   zcx_aap_illegal_argument,
      "! Create from class or interface name
      "! @parameter iv_name | Class / interface name
      "! @parameter iv_skip_relevance_check | Do not check if the object implements interface
      "!                                      ZIF_AAP_ANNOTATABLE
      "! @parameter ro_processor | Processor instance
      "! @raising zcx_aap_class_not_annotatable | iv_name does not resolve to a class or an
      "!                                          interface that implements ZIF_AAP_ANNOTATABLE
      "! @raising zcx_aap_illegal_argument | iv_name does not resolve to a class or an interface
      from_name IMPORTING iv_name                 TYPE abap_classname
                          iv_skip_relevance_check TYPE abap_bool DEFAULT abap_false
                RETURNING VALUE(ro_processor)     TYPE REF TO zcl_aap_proc_object
                RAISING   zcx_aap_class_not_annotatable
                          zcx_aap_illegal_argument,
      "! Create from object descriptor
      "! @parameter io_descr | Object descriptor describing a class or interface that implements
      "!                       ZIF_AAP_ANNOTATABLE
      "! @parameter iv_skip_relevance_check | Do not check if the object implements interface
      "!                                      ZIF_AAP_ANNOTATABLE
      "! @parameter ro_processor | Processor instance
      "! @raising zcx_aap_class_not_annotatable | io_descr does not describe a class or
      "!                                          interface that implements ZIF_AAP_ANNOTATABLE
      "! @raising zcx_aap_illegal_argument | io_descr is null
      from_descriptor IMPORTING io_descr                TYPE REF TO cl_abap_objectdescr
                                iv_skip_relevance_check TYPE abap_bool DEFAULT abap_false
                      RETURNING VALUE(ro_processor)     TYPE REF TO zcl_aap_proc_object
                      RAISING   zcx_aap_class_not_annotatable
                                zcx_aap_illegal_argument,
      "! Check if object is relevant for annotation processing
      "! @parameter io_object | Object reference
      "! @parameter rv_relevant | Object is relevant
      "! @raising zcx_aap_illegal_argument | io_object cannot be null
      is_object_relevant_by_ref IMPORTING io_object          TYPE REF TO object
                                RETURNING VALUE(rv_relevant) TYPE abap_bool
                                RAISING   zcx_aap_illegal_argument,
      "! Check if object is relevant for annotation processing
      "! @parameter io_descr | Object descriptor
      "! @parameter rv_relevant | Object is relevant
      "! @raising zcx_aap_illegal_argument | io_descr cannot be null
      is_object_relevant_by_descr IMPORTING io_descr           TYPE REF TO cl_abap_objectdescr
                                  RETURNING VALUE(rv_relevant) TYPE abap_bool
                                  RAISING   zcx_aap_illegal_argument,
      "! Check if object is relevant for annotation processing
      "! @parameter iv_name | Class / interface name
      "! @parameter rv_relevant | Object is relevant
      "! @raising zcx_aap_illegal_argument | iv_name does not resolve to a class or interface
      is_object_relevant_by_name IMPORTING iv_name            TYPE abap_classname
                                 RETURNING VALUE(rv_relevant) TYPE abap_bool
                                 RAISING   zcx_aap_illegal_argument.
    METHODS:
      "! Get method processor by name
      "! @parameter iv_method_name | Method name
      "! @parameter ro_processor | Found method processor
      "! @raising zcx_aap_illegal_argument | Method does not exist
      get_method_processor IMPORTING iv_method_name      TYPE abap_methname
                           RETURNING VALUE(ro_processor) TYPE REF TO zcl_aap_proc_method
                           RAISING   zcx_aap_illegal_argument,
      "! Get attribute processor by name
      "! @parameter iv_attribute_name | Attribute name
      "! @parameter ro_processor | Found attribute processor
      "! @raising zcx_aap_illegal_argument | Attribute does not exist
      get_attribute_processor IMPORTING iv_attribute_name   TYPE abap_attrname
                              RETURNING VALUE(ro_processor) TYPE REF TO zcl_aap_proc_attribute
                              RAISING   zcx_aap_illegal_argument,
      "! Get all method processors
      "! @parameter rt_processors | Method processors
      get_method_processors RETURNING VALUE(rt_processors) TYPE gty_method_proc_tab,
      "! Get all attribute processors
      "! @parameter rt_processors | Attribute processors
      get_attribute_processors RETURNING VALUE(rt_processors) TYPE gty_attribute_proc_tab,
      load_all REDEFINITION,
      get_annotations REDEFINITION,
      is_annotatable REDEFINITION.
    DATA:
      "! Object descriptor
      mo_object_descr       TYPE REF TO cl_abap_objectdescr READ-ONLY,
      "! Relative classname
      mv_classname_relative TYPE abap_classname READ-ONLY,
      "! Absolute classname
      mv_classname_absolute TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING io_descriptor TYPE REF TO cl_abap_objectdescr,
      load_methods,
      load_attributes.
    CLASS-DATA:
      gt_object_kind_range TYPE RANGE OF abap_typecategory.
    DATA:
      mt_method_processor_cache    TYPE gty_method_proc_tab,
      mt_attribute_processor_cache TYPE gty_attribute_proc_tab.
ENDCLASS.



CLASS zcl_aap_proc_object IMPLEMENTATION.
  METHOD class_constructor.
    gt_object_kind_range = VALUE #(
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>kind_class )
      ( sign = 'I' option = 'EQ' low = cl_abap_typedescr=>kind_intf )
    ).
  ENDMETHOD.

  METHOD from_name.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name    " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_descr)    " Reference to description object
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).

    " Check if type descriptor could be fetched and if it describes an interface or a class
    IF sy-subrc <> 0 OR lo_descr IS NOT BOUND OR lo_descr->kind NOT IN gt_object_kind_range.
      MESSAGE e012(zaap) WITH iv_name INTO DATA(lv_reason).
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = zcx_aap_illegal_argument=>gc_with_name_and_reason
          iv_name   = 'IV_NAME'
          iv_reason = lv_reason
          iv_value  = iv_name.
    ENDIF.

    ro_processor = from_descriptor( io_descr                = CAST #( lo_descr )
                                    iv_skip_relevance_check = iv_skip_relevance_check ).
  ENDMETHOD.

  METHOD from_object.
    ro_processor = from_descriptor( CAST #(
                     cl_abap_typedescr=>describe_by_object_ref( ii_annotatable )
                   ) ).
  ENDMETHOD.

  METHOD from_descriptor.
    zcx_aap_illegal_argument=>raise_if_nullpointer( io_ref  = io_descr
                                                    iv_name = 'IO_DESCR' ) ##NO_TEXT.

    IF iv_skip_relevance_check = abap_false
        AND is_object_relevant_by_descr( io_descr ) = abap_false.

      RAISE EXCEPTION TYPE zcx_aap_class_not_annotatable
        EXPORTING
          iv_name      = 'IO_DESCR'
          iv_classname = CONV #( io_descr->get_relative_name( ) ) ##NO_TEXT.
    ENDIF.

    ro_processor = NEW #( io_descr ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    " Private constructor, validation using user friendly exceptions is done in factory methods
    ASSERT io_descriptor IS BOUND.

    mo_object_descr = io_descriptor.
    mv_classname_absolute = io_descriptor->absolute_name.
    mv_classname_relative = io_descriptor->get_relative_name( ).
  ENDMETHOD.

  METHOD get_annotations.
    " TODO: Think of propagating the exception in some way
    rt_annotations = get_resolver( )->get_annotations_for_object( mv_classname_relative ).
  ENDMETHOD.

  METHOD get_attribute_processor.
    DATA(lt_processors) = get_attribute_processors( ).

    TRY.
        ro_processor = lt_processors[ attribute_name = iv_attribute_name ]-processor.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        MESSAGE e014(zaap) WITH iv_attribute_name INTO DATA(lv_reason).
        RAISE EXCEPTION TYPE zcx_aap_illegal_argument
          EXPORTING
            is_textid   = zcx_aap_illegal_argument=>gc_with_name_and_reason
            ix_previous = lx_ex
            iv_name     = 'IV_ATTRIBUTE_NAME'
            iv_reason   = lv_reason
            iv_value    = iv_attribute_name ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD get_attribute_processors.
    " On demand lazy loading of processors
    IF lines( mt_attribute_processor_cache ) = 0.
      load_attributes( ).
    ENDIF.

    rt_processors = mt_attribute_processor_cache.
  ENDMETHOD.

  METHOD get_method_processor.
    DATA(lt_processors) = get_method_processors( ).

    TRY.
        ro_processor = lt_processors[ method_name = iv_method_name ]-processor.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        MESSAGE e013(zaap) WITH iv_method_name INTO DATA(lv_reason).
        RAISE EXCEPTION TYPE zcx_aap_illegal_argument
          EXPORTING
            is_textid   = zcx_aap_illegal_argument=>gc_with_name_and_reason
            ix_previous = lx_ex
            iv_name     = 'IV_METHOD_NAME'
            iv_reason   = lv_reason
            iv_value    = iv_method_name ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD get_method_processors.
    " On demand lazy loading of processors
    IF lines( mt_method_processor_cache ) = 0.
      load_methods( ).
    ENDIF.

    rt_processors = mt_method_processor_cache.
  ENDMETHOD.

  METHOD is_object_relevant_by_descr.
    zcx_aap_illegal_argument=>raise_if_nullpointer( io_ref  = io_descr
                                                    iv_name = 'IO_DESCR' ) ##NO_TEXT.

    " A class / interface is relevant for annotation processing if it or one of its base classes
    " implements the empty marker interface ZIF_AAP_ANNOTATABLE

    rv_relevant = COND #( " Check if it is a direct interface reference to ZIF_AAP_ANNOTATABLE
                          WHEN io_descr->kind = cl_abap_typedescr=>kind_intf
                               AND io_descr->get_relative_name( ) = gc_annotatable_intf_name
                               THEN abap_true
                          " Otherwise the interface must be a component of the class / interface
                          ELSE boolc( line_exists(
                                        io_descr->interfaces[ name = gc_annotatable_intf_name ]
                                      ) ) ).
  ENDMETHOD.

  METHOD is_object_relevant_by_name.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name    " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_descr)    " Reference to description object
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).

    " Check if type descriptor could be fetched and if it describes an interface or a class
    IF sy-subrc <> 0 OR lo_descr IS NOT BOUND OR lo_descr->kind NOT IN gt_object_kind_range.
      MESSAGE e012(zaap) WITH iv_name INTO DATA(lv_reason).
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument
        EXPORTING
          is_textid = zcx_aap_illegal_argument=>gc_with_name_and_reason
          iv_name   = 'IV_CLASSNAME'
          iv_reason = lv_reason
          iv_value  = iv_name ##NO_TEXT.
    ENDIF.

    rv_relevant = is_object_relevant_by_descr( CAST #( lo_descr ) ).
  ENDMETHOD.

  METHOD is_object_relevant_by_ref.
    zcx_aap_illegal_argument=>raise_if_nullpointer( io_ref  = io_object
                                                    iv_name  = 'IO_OBJECT' ) ##NO_TEXT.

    rv_relevant = is_object_relevant_by_descr( CAST #(
                    cl_abap_typedescr=>describe_by_object_ref( io_object )
                  ) ).
  ENDMETHOD.

  METHOD load_all.
    IF lines( mt_attribute_processor_cache ) = 0.
      load_attributes( ).
    ENDIF.

    LOOP AT mt_attribute_processor_cache ASSIGNING FIELD-SYMBOL(<ls_attr_proc>).
      <ls_attr_proc>-processor->load_all( ).
    ENDLOOP.

    IF lines( mt_method_processor_cache ) = 0.
      load_methods( ).
    ENDIF.

    LOOP AT mt_method_processor_cache ASSIGNING FIELD-SYMBOL(<ls_meth_proc>).
      <ls_meth_proc>-processor->load_all( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD load_attributes.
    LOOP AT mo_object_descr->attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
      INSERT VALUE #( attribute_name = <ls_attribute>-name
                      processor      = NEW zcl_aap_proc_attribute(
                                           is_attribute_description  = <ls_attribute>
                                           iv_containing_object_name = mv_classname_relative
                                           io_object_processor       = me
                                       )
                      ) INTO TABLE mt_attribute_processor_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD load_methods.
    LOOP AT mo_object_descr->methods ASSIGNING FIELD-SYMBOL(<ls_method>).
      INSERT VALUE #( method_name = <ls_method>-name
                      processor   = NEW zcl_aap_proc_method(
                                        is_methdescr              = <ls_method>
                                        iv_containing_object_name = mv_classname_relative
                                        io_object_processor       = me
                                    )
                      ) INTO TABLE mt_method_processor_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_annotatable.
    rv_annotatable = is_object_relevant_by_descr( mo_object_descr ).
  ENDMETHOD.
ENDCLASS.