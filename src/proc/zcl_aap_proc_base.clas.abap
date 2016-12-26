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
      "! Get an annotation instance by its name
      "! @parameter iv_classname | Annotation class name
      "! @parameter ro_annotation | Found annotation class instance
      "! @raising zcx_aap_illegal_argument | Annotation class not present
      get_annotation_by_name IMPORTING iv_classname         TYPE abap_classname
                             RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                             RAISING   zcx_aap_illegal_argument,
      "! Get an annotation instance by descriptor
      "! @parameter io_descr | Descriptor instance
      "! @parameter ro_annotation | Found annotation class instance
      "! @raising zcx_aap_illegal_argument | io_descr cannot be null or annotation not present
      get_annotation_by_descr IMPORTING io_descr             TYPE REF TO cl_abap_classdescr
                              RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                              RAISING   zcx_aap_illegal_argument,
      "! Get all annotations directly associated to this processor
      "! @parameter rt_annotations | Associated annotations
      get_annotations ABSTRACT RETURNING VALUE(rt_annotations) TYPE zif_aap_annotation_resolver=>gty_annotation_tab,
      "! Force populate internal caches recursively (disables loading on demand)
      load_all ABSTRACT.
  PROTECTED SECTION.
    CONSTANTS:
      gc_annotatable_intf_name TYPE abap_intfname VALUE 'ZIF_AAP_ANNOTATABLE'.
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
        MESSAGE e018(zaap) WITH io_descr->get_relative_name( ) INTO DATA(lv_msg).
        RAISE EXCEPTION TYPE zcx_aap_illegal_argument
          EXPORTING
            is_textid   = zcx_aap_illegal_argument=>gc_with_name_and_reason
            ix_previous = lx_ex
            iv_name     = 'IO_DESCR'
            iv_reason   = lv_msg ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD get_annotation_by_name.
    DATA(lt_annotations) = get_annotations( ).

    TRY.
        ro_annotation = lt_annotations[ classname = iv_classname ]-instance.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        MESSAGE e018(zaap) WITH iv_classname INTO DATA(lv_msg).
        RAISE EXCEPTION TYPE zcx_aap_illegal_argument
          EXPORTING
            is_textid   = zcx_aap_illegal_argument=>gc_with_name_and_reason
            ix_previous = lx_ex
            iv_name     = 'IV_CLASSNAME'
            iv_reason   = lv_msg
            iv_value    = iv_classname ##NO_TEXT.
    ENDTRY.
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
ENDCLASS.