"! Annotation base class
CLASS zcl_aap_annotation_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED
  GLOBAL FRIENDS zif_aap_annotation_resolver.

  PUBLIC SECTION.
    TYPES:
      gty_target_list TYPE HASHED TABLE OF REF TO zcl_aap_annotation_target
                           WITH UNIQUE KEY table_line.
    METHODS:
      "! Get possible targets of this annotation
      "! @parameter rt_targets | Targets
      get_targets FINAL RETURNING VALUE(rt_targets) TYPE gty_target_list.
  PROTECTED SECTION.
    TYPES:
      gty_attribute_tab     TYPE SORTED TABLE OF abap_attrname WITH UNIQUE KEY table_line,
      gty_attribute_ref     TYPE REF TO data,
      gty_attribute_ref_tab TYPE HASHED TABLE OF gty_attribute_ref WITH UNIQUE KEY table_line.
    METHODS:
      constructor,
      "! Get all attributes that should be bindable
      "! @parameter rt_attributes | Bindable attributes
      get_bindable_attributes RETURNING VALUE(rt_attributes) TYPE gty_attribute_tab,
      "! Internal definition of possible annotation targets
      "! @parameter rt_targets | Targets
      get_targets_internal RETURNING VALUE(rt_targets) TYPE gty_target_list,
      "! Build a bindable attributes list by data references
      "! <p>
      "! Ensures that refactoring of member variable names also works here as the attributes are
      "! not defined as literals. However, performance will be slower.
      "! </p>
      "! @parameter it_attribute_refs | References to attributes
      "! @parameter io_instance | Self reference of the concrete annotation class
      "! @parameter rt_attributes | Built bindable attributes table
      build_attribute_list FINAL IMPORTING it_attribute_refs    TYPE gty_attribute_ref_tab
                                           io_instance          TYPE REF TO zcl_aap_annotation_base
                                 RETURNING VALUE(rt_attributes) TYPE gty_attribute_tab.
  PRIVATE SECTION.
    DATA:
      mt_targets TYPE gty_target_list.
ENDCLASS.



CLASS ZCL_AAP_ANNOTATION_BASE IMPLEMENTATION.


  METHOD build_attribute_list.
    DATA(lo_descr) = CAST cl_abap_classdescr(
                       cl_abap_typedescr=>describe_by_object_ref( io_instance )
                     ).

    LOOP AT it_attribute_refs ASSIGNING FIELD-SYMBOL(<lr_attribute>).
      LOOP AT lo_descr->attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>)
           WHERE visibility = cl_abap_objectdescr=>public.

        ASSIGN io_instance->(<ls_attribute>-name) TO FIELD-SYMBOL(<lg_attribute>).
        IF REF #( <lg_attribute> ) = <lr_attribute>.
          INSERT <ls_attribute>-name INTO TABLE rt_attributes.
          EXIT.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    ASSERT lines( it_attribute_refs ) = lines( rt_attributes ).
  ENDMETHOD.


  METHOD constructor.
    mt_targets = get_targets_internal( ).
  ENDMETHOD.


  METHOD get_bindable_attributes ##NEEDED.
    " Should be overwritten in subclasses, if they have bindable attributes
  ENDMETHOD.


  METHOD get_targets.
    rt_targets = mt_targets.
  ENDMETHOD.


  METHOD get_targets_internal.
    " Should be overwritten in subclasses, if they have other targets than these default ones
    rt_targets = VALUE #(
      ( zcl_aap_annotation_target=>go_attribute )
      ( zcl_aap_annotation_target=>go_object )
      ( zcl_aap_annotation_target=>go_parameter )
      ( zcl_aap_annotation_target=>go_method )
    ).
  ENDMETHOD.
ENDCLASS.