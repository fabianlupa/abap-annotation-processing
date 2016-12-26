"! Default annotation resolver (using database tables)
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
      gty_mapping_result_tab TYPE STANDARD TABLE OF gty_mapping_result WITH DEFAULT KEY,
      BEGIN OF gty_query_cache,
        query  TYPE gty_mapping,
        result TYPE gty_mapping_result_tab,
      END OF gty_query_cache,
      gty_query_cache_tab TYPE HASHED TABLE OF gty_query_cache WITH UNIQUE KEY query.
    CLASS-METHODS:
      select_entries IMPORTING is_key            TYPE gty_mapping
                     RETURNING VALUE(rt_entries) TYPE gty_mapping_result_tab,
      build_annotation_tab IMPORTING it_entries            TYPE gty_mapping_result_tab
                           RETURNING VALUE(rt_annotations) TYPE zif_aap_annotation_resolver=>gty_annotation_tab
                           RAISING   zcx_aap_incons_customizing.
    CLASS-DATA:
      gt_query_cache TYPE gty_query_cache_tab.
ENDCLASS.



CLASS zcl_aap_db_annotation_resolver IMPLEMENTATION.
  METHOD select_entries.
    TYPES: BEGIN OF lty_result.
        INCLUDE TYPE gty_mapping.
    TYPES: detailid       TYPE zaap_l_detailid,
           annotationname TYPE seoclsname,
           END OF lty_result.
    DATA: lt_result    TYPE STANDARD TABLE OF lty_result.

    " Check if cache contains the result
    TRY.
        rt_entries = gt_query_cache[ query = is_key ]-result.
        RETURN.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
        " Cache did not contain the result
    ENDTRY.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE @lt_result
      FROM zaap_tcassociat
      WHERE objectname = @is_key-objectname
        AND attrname   = @is_key-attrname
        AND methname   = @is_key-methname
        AND parmname   = @is_key-parmname
      ORDER BY PRIMARY KEY.

    rt_entries = CORRESPONDING #( lt_result ).

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_entry>).
      ASSIGN rt_entries[ sy-tabix ]-parameters TO FIELD-SYMBOL(<lt_parameters>).
      ASSERT <lt_parameters> IS ASSIGNED.

      SELECT attrname, value INTO TABLE @<lt_parameters>
        FROM zaap_tcassocdet
        WHERE detailid = @<ls_entry>-detailid
        ORDER BY PRIMARY KEY.
    ENDLOOP.

    " Cache the result (for this internal session)
    INSERT VALUE #( query = is_key result = rt_entries ) INTO TABLE gt_query_cache.
  ENDMETHOD.


  METHOD get_annotations_for_attribute.
    DATA(lt_entries) = select_entries( VALUE #( objectname = iv_containing_object_name
                                                attrname   = iv_attribute_name ) ).
    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.


  METHOD get_annotations_for_method.
    DATA(lt_entries) = select_entries( VALUE #( objectname = iv_containing_object_name
                                                methname   = iv_method_name ) ).
    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.


  METHOD get_annotations_for_object.
    DATA(lt_entries) = select_entries( VALUE #( objectname = iv_name ) ).
    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.


  METHOD get_annotations_for_parameter.
    DATA(lt_entries) = select_entries( VALUE #( objectname = iv_containing_object_name
                                                methname   = iv_containing_method_name
                                                parmname   = iv_parameter_name ) ).
    rt_annotations = build_annotation_tab( lt_entries ).
  ENDMETHOD.

  METHOD build_annotation_tab.
    DATA: lo_annotation TYPE REF TO zcl_aap_annotation_base.

    LOOP AT it_entries ASSIGNING FIELD-SYMBOL(<ls_entry>).
      CREATE OBJECT lo_annotation TYPE (<ls_entry>-annotationname).
      " TODO: Catch instantiation error because of private constructor or one with parameters
      ASSERT lo_annotation IS BOUND.

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