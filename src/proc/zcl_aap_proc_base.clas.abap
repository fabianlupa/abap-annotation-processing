"! Processor base class
CLASS zcl_aap_proc_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    METHODS:
      "! Check if an annotation is present by classname
      "! @parameter iv_classname | Annotation class name
      "! @parameter rv_present | Annotation is present
      is_annotation_present_by_name IMPORTING iv_classname      TYPE abap_classname
                                    RETURNING VALUE(rv_present) TYPE abap_bool,
      "! Check if an annotation is present by descriptor
      "! @parameter io_descr | Descriptor instance
      "! @parameter rv_present | Annotation is present
      "! @raising zcx_aap_illegal_argument | io_descr cannot be null
      is_annotation_present_by_descr IMPORTING io_descr          TYPE REF TO cl_abap_classdescr
                                     RETURNING VALUE(rv_present) TYPE abap_bool
                                     RAISING   zcx_aap_illegal_argument,
      "! Check if an annotation is present by data variable
      "! @parameter ig_data | Variable typed as REF TO annotation class
      "! @parameter rv_present | Annotation is present
      "! @raising zcx_aap_illegal_argument | ig_data is not a reference variable to an annotation
      is_annotation_present_by_data IMPORTING ig_data           TYPE any
                                    RETURNING VALUE(rv_present) TYPE abap_bool
                                    RAISING   zcx_aap_illegal_argument,
      "! Get an annotation instance by its name
      "! @parameter iv_classname | Annotation class name
      "! @parameter ro_annotation | Found annotation class instance
      "! @raising zcx_aap_annotation_not_present | Annotation is not present
      get_annotation_by_name IMPORTING iv_classname         TYPE abap_classname
                             RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                             RAISING   zcx_aap_annotation_not_present,
      "! Get an annotation instance by descriptor
      "! @parameter io_descr | Descriptor instance
      "! @parameter ro_annotation | Found annotation class instance
      "! @raising zcx_aap_illegal_argument | io_descr cannot be null
      "! @raising zcx_aap_annotation_not_present | Annotation is not present
      get_annotation_by_descr IMPORTING io_descr             TYPE REF TO cl_abap_classdescr
                              RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                              RAISING   zcx_aap_illegal_argument
                                        zcx_aap_annotation_not_present,
      "! Get an annotation instance by data variable
      "! @parameter ig_data | Variable typed as REF TO annotation class
      "! @parameter ro_annotation | Found annotation class instance
      "! @raising zcx_aap_illegal_argument | ig_data is not a reference variable to an annotation
      "! @raising zcx_aap_annotation_not_present | Annotation is not present
      get_annotation_by_data IMPORTING ig_data              TYPE any
                             RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                             RAISING   zcx_aap_illegal_argument
                                       zcx_aap_annotation_not_present,
      "! Get all annotations directly associated to this processor
      "! @parameter rt_annotations | Associated annotations
      get_annotations ABSTRACT RETURNING VALUE(rt_annotations) TYPE zif_aap_annotation_resolver=>gty_annotation_tab,
      "! Force populate internal caches recursively (disables loading on demand)
      load_all ABSTRACT,
      "! Are annnotations present?
      "! @parameter rv_has_annotations | Annotations are present
      has_annotations RETURNING VALUE(rv_has_annotations) TYPE abap_bool,
      "! How many annotations are present
      "! @parameter rv_count | Annotation count
      get_annotation_count RETURNING VALUE(rv_count) TYPE i,
      "! Can this class / interface / method / parameter / attribute be annotated
      "! @parameter rv_annotatable | Annotatable
      is_annotatable ABSTRACT RETURNING VALUE(rv_annotatable) TYPE abap_bool,
      "! Get the target for this processor
      "! @parameter ro_target | Target enumeration instance
      get_target ABSTRACT RETURNING VALUE(ro_target) TYPE REF TO zcl_aap_annotation_target.
  PROTECTED SECTION.
    CONSTANTS:
      gc_annotatable_intf_name TYPE abap_intfname VALUE zif_aap_annotatable=>gc_intfname.
    METHODS:
      constructor,
      "! Get the annotation resolver
      "! @parameter ri_resolver | Resolver instance
      get_resolver FINAL RETURNING VALUE(ri_resolver) TYPE REF TO zif_aap_annotation_resolver.
  PRIVATE SECTION.
    DATA:
      mi_resolver TYPE REF TO zif_aap_annotation_resolver.
ENDCLASS.



CLASS zcl_aap_proc_base IMPLEMENTATION.
  METHOD constructor.
    mi_resolver = zcl_aap_resolver_injector=>get_resolver( ).
  ENDMETHOD.

  METHOD get_resolver.
    ri_resolver = mi_resolver.
  ENDMETHOD.

  METHOD get_annotation_by_descr.
    DATA(lt_annotations) = get_annotations( ).

    zcx_aap_illegal_argument=>raise_if_nullpointer( iv_name = 'IO_DESCR'
                                                    io_ref  = io_descr ) ##NO_TEXT.

    TRY.
        ro_annotation = lt_annotations[ descriptor = io_descr ]-instance.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_aap_annotation_not_present
          EXPORTING
            ix_previous = lx_ex
            iv_name     = CONV #( io_descr->get_relative_name( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_annotation_by_name.
    DATA(lt_annotations) = get_annotations( ).

    TRY.
        ro_annotation = lt_annotations[ classname = iv_classname ]-instance.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_aap_annotation_not_present
          EXPORTING
            ix_previous = lx_ex
            iv_name     = iv_classname.
    ENDTRY.
  ENDMETHOD.

  METHOD get_annotation_by_data.
    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( ig_data ).

    IF lo_descr->type_kind <> cl_abap_typedescr=>kind_ref.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument. " TODO: Add exception message
    ENDIF.

    DATA(lo_referenced_descr) = CAST cl_abap_refdescr( lo_descr )->get_referenced_type( ).
    IF lo_referenced_descr->type_kind <> cl_abap_typedescr=>kind_class.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument. " TODO: Add exception message
    ENDIF.

    ro_annotation = get_annotation_by_descr( CAST cl_abap_classdescr( lo_referenced_descr ) ).
  ENDMETHOD.

  METHOD is_annotation_present_by_descr.
    DATA(lt_annotations) = get_annotations( ).

    zcx_aap_illegal_argument=>raise_if_nullpointer( iv_name = 'IO_DESCR'
                                                    io_ref  = io_descr ) ##NO_TEXT.

    rv_present = boolc( line_exists( lt_annotations[ descriptor = io_descr ] ) ).
  ENDMETHOD.

  METHOD is_annotation_present_by_name.
    DATA(lt_annotations) = get_annotations( ).
    rv_present = boolc( line_exists( lt_annotations[ classname = iv_classname ] ) ).
  ENDMETHOD.

  METHOD is_annotation_present_by_data.
    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( ig_data ).

    IF lo_descr->type_kind <> cl_abap_typedescr=>kind_ref.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument. " TODO: Add exception message
    ENDIF.

    DATA(lo_referenced_descr) = CAST cl_abap_refdescr( lo_descr )->get_referenced_type( ).
    IF lo_referenced_descr->type_kind <> cl_abap_typedescr=>kind_class.
      RAISE EXCEPTION TYPE zcx_aap_illegal_argument. " TODO: Add exception message
    ENDIF.

    rv_present = is_annotation_present_by_descr( CAST cl_abap_classdescr( lo_referenced_descr ) ).
  ENDMETHOD.

  METHOD get_annotation_count.
    rv_count = lines( get_annotations( ) ).
  ENDMETHOD.

  METHOD has_annotations.
    rv_has_annotations = boolc( lines( get_annotations( ) ) > 0 ).
  ENDMETHOD.
ENDCLASS.