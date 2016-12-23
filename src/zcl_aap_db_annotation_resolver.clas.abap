"! Default annotation resolver
CLASS zcl_aap_db_annotation_resolver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_aap_annotation_resolver.
    ALIASES:
      get_annotations_for_attribute FOR zif_aap_annotation_resolver~get_annotations_for_attribute,
      get_annotations_for_method FOR zif_aap_annotation_resolver~get_annotations_for_method,
      get_annotations_for_object FOR zif_aap_annotation_resolver~get_annotations_for_object,
      get_annotations_for_parameter FOR zif_aap_annotation_resolver~get_annotations_for_parameter.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_tabname_classes    TYPE tabname VALUE 'ZAAP_TCCLSASSOC',
      gc_tabname_attributes TYPE tabname VALUE 'ZAAP_TCATTASSOC',
      gc_tabname_methods    TYPE tabname VALUE 'ZAAP_TCMTHASSOC',
      gc_tabname_parameters TYPE tabname VALUE 'ZAAP_TCPARASSOC'.
    TYPES:
      BEGIN OF gty_parameter,
        name  TYPE abap_attrname,
        value TYPE string,
      END OF gty_parameter,
      BEGIN OF gty_mapping,
        objectname TYPE abap_classname,
        attrname   TYPE abap_attrname,
        methname   TYPE abap_methname,
        parmname   TYPE abap_parmname,
      END OF gty_mapping,
      BEGIN OF gty_mapping_result,
        objectname     TYPE abap_classname,
        attrname       TYPE abap_attrname,
        methname       TYPE abap_methname,
        parmname       TYPE abap_parmname,
        annotationname TYPE abap_classname,
        parameters     TYPE SORTED TABLE OF gty_parameter WITH UNIQUE KEY name,
      END OF gty_mapping_result,
      gty_mapping_result_tab TYPE STANDARD TABLE OF gty_mapping_result WITH DEFAULT KEY.
    CLASS-METHODS:
      select_entries IMPORTING iv_header_table   TYPE tabname
                               is_key            TYPE gty_mapping
                     RETURNING VALUE(rt_entries) TYPE gty_mapping_result_tab,
      build_annotation_tab IMPORTING it_entries            TYPE gty_mapping_result_tab
                           RETURNING VALUE(rt_annotations) TYPE zif_aap_annotation_resolver=>gty_annotation_tab
                           RAISING   zcx_aap_incons_customizing.
ENDCLASS.



CLASS zcl_aap_db_annotation_resolver IMPLEMENTATION.


  METHOD select_entries.
    TYPES: BEGIN OF lty_result.
        INCLUDE TYPE gty_mapping.
    TYPES: detailid       TYPE zaap_l_detailid,
           annotationname TYPE seoclsname,
           END OF lty_result.
    DATA: lt_result    TYPE STANDARD TABLE OF lty_result,
          lt_condition TYPE stringtab,
          lv_condition TYPE string.

    IF is_key-objectname IS NOT INITIAL.
      APPEND |objectname = '{ is_key-objectname }'| TO lt_condition.
    ENDIF.

    IF is_key-methname IS NOT INITIAL.
      APPEND |methname = '{ is_key-methname }'| TO lt_condition.
    ENDIF.

    IF is_key-attrname IS NOT INITIAL.
      APPEND |attrname = '{ is_key-attrname }'| TO lt_condition.
    ENDIF.

    IF is_key-parmname IS NOT INITIAL.
      APPEND |parmname = '{ is_key-parmname }'| TO lt_condition.
    ENDIF.

    IF lines( lt_condition ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE LINES OF lt_condition INTO lv_condition SEPARATED BY ` AND `.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE @lt_result
      FROM (iv_header_table)
      WHERE (lv_condition)
      ORDER BY PRIMARY KEY.

    rt_entries = CORRESPONDING #( lt_result ).

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_entry>).
      ASSIGN rt_entries[ sy-tabix ]-parameters TO FIELD-SYMBOL(<lt_parameters>).

      SELECT attrname, value INTO TABLE @<lt_parameters>
        FROM zaap_tcassocdet
        WHERE detailid = @<ls_entry>-detailid
        ORDER BY PRIMARY KEY.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_annotations_for_attribute.
    DATA(lt_entries) = select_entries(
        iv_header_table = gc_tabname_attributes
        is_key          = VALUE #( objectname = iv_containing_object_name
                                   attrname   = iv_attribute_name )
    ).

    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.


  METHOD get_annotations_for_method.
    DATA(lt_entries) = select_entries(
        iv_header_table = gc_tabname_methods
        is_key          = VALUE #( objectname = iv_containing_object_name
                                   methname   = iv_method_name )
    ).

    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.


  METHOD get_annotations_for_object.
    DATA(lt_entries) = select_entries(
        iv_header_table = gc_tabname_classes
        is_key          = VALUE #( objectname = iv_name )
    ).

    rt_annotations = build_annotation_tab( lt_entries ).

*    TYPES: BEGIN OF lty_selection,
*             objectname     TYPE seoclsname,
*             annotationname TYPE seoclsname,
*             detailid       TYPE zaap_l_detailid,
*             attrname       TYPE seoattname,
*             value          TYPE string,
*           END OF lty_selection.
*    DATA: lt_selection  TYPE STANDARD TABLE OF lty_selection,
*          lo_annotation TYPE REF TO zcl_aap_annotation_base.
*
**    DATA(lt_selection) = select_entries( is_key          = VALUE #( classname = iv_name )
**                                         iv_header_table = gc_tabname_classes ).
*    SELECT h~objectname, h~annotationname, h~detailid, d~attrname, d~value
*      INTO CORRESPONDING FIELDS OF TABLE @lt_selection
*      FROM zaap_tcclsassoc AS h
*      LEFT JOIN zaap_tcassocdet AS d
*      ON h~detailid = d~detailid
*      WHERE h~objectname = @iv_name
*      ORDER BY h~annotationname, d~detailid, d~attrname.
*
*    LOOP AT lt_selection ASSIGNING FIELD-SYMBOL(<ls_line>)
*         GROUP BY ( annotationname = <ls_line>-annotationname )
*         ASCENDING
*         REFERENCE INTO DATA(lr_group).
*
**     LOOP AT lt_selection ASSIGNING FIELD-SYMBOL(<ls_line>)
**          GROUP BY ( annotationname = <ls_line>-annotation )
**          ASCENDING
**          REFERENCE INTO DATA(lr_group).
*
*      CREATE OBJECT lo_annotation TYPE (lr_group->annotationname).
*
*      LOOP AT GROUP lr_group ASSIGNING FIELD-SYMBOL(<ls_parameter>).
*        DATA(lt_bindable_attributes) = lo_annotation->get_bindable_attributes( ).
*
*        IF line_exists( lt_bindable_attributes[ table_line = <ls_parameter>-attrname ] ).
*          ASSIGN lo_annotation->(<ls_parameter>-attrname) TO FIELD-SYMBOL(<lg_attribute>).
*          " If the annotation class does not grant friendship to ZIF_AAP_ANNOTATION_RESOLVER and
*          " the attribute is read-only or not public the following statement will raise an
*          " uncatchable error of type MOVE_TO_LIT_NOTALLOWED_NODATA and write a system dump.
*          " TODO: Find a way to catch the error or check beforehand if you can write to the field
*          "       symbol.
*          <lg_attribute> = <ls_parameter>-value.
*
*        ELSE.
*          " There is an attribute that is specified with a value in the database but is not bindable
*          RAISE EXCEPTION TYPE zcx_aap_incons_customizing.
*        ENDIF.
*
*        CLEAR lt_bindable_attributes.
*        UNASSIGN <lg_attribute>.
*      ENDLOOP.
*
*      UNASSIGN: <ls_parameter>.
*
*      INSERT VALUE #(
*        classname  = lr_group->annotationname
*        descriptor = CAST #( cl_abap_typedescr=>describe_by_object_ref( lo_annotation ) )
*        instance   = lo_annotation
*      ) INTO TABLE rt_annotations.
*
*      FREE lo_annotation.
*    ENDLOOP.
  ENDMETHOD.


  METHOD get_annotations_for_parameter.
    DATA(lt_entries) = select_entries(
        iv_header_table = gc_tabname_parameters
        is_key          = VALUE #( objectname = iv_containing_object_name
                                   methname   = iv_containing_method_name
                                   parmname   = iv_parameter_name )
    ).

    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.

  METHOD build_annotation_tab.
    LOOP AT it_entries ASSIGNING FIELD-SYMBOL(<ls_entry>).
      DATA: lo_annotation TYPE REF TO zcl_aap_annotation_base.
      CREATE OBJECT lo_annotation TYPE (<ls_entry>-annotationname).

      DATA(lt_bindable_attributes) = lo_annotation->get_bindable_attributes( ).

      LOOP AT <ls_entry>-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
        IF NOT line_exists( lt_bindable_attributes[ table_line = <ls_parameter>-name ] ).
          RAISE EXCEPTION TYPE zcx_aap_incons_customizing.
        ENDIF.

        ASSIGN lo_annotation->(<ls_parameter>-name) TO FIELD-SYMBOL(<lg_attribute>).
        ASSERT <lg_attribute> IS ASSIGNED.

        " If the annotation class does not grant friendship to ZIF_AAP_ANNOTATION_RESOLVER and
        " the attribute is read-only or not public the following statement will raise an
        " uncatchable error of type MOVE_TO_LIT_NOTALLOWED_NODATA and write a system dump.
        " TODO: Find a way to catch the error or check beforehand if you can write to the field
        "       symbol.
        <lg_attribute> = <ls_parameter>-value.
        UNASSIGN <lg_attribute>.
      ENDLOOP.

      INSERT VALUE #( classname  = <ls_entry>-annotationname
                      descriptor = CAST #( cl_abap_typedescr=>describe_by_object_ref(
                                             lo_annotation
                                           ) )
                      instance   = lo_annotation ) INTO TABLE rt_annotations.

      FREE lo_annotation.
      CLEAR lt_bindable_attributes.
      UNASSIGN <ls_parameter>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.