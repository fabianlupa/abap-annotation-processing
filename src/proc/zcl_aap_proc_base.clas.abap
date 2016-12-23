CLASS zcl_aap_proc_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    METHODS:
      "! Check if an annotation is present by classname
      "! @parameter iv_classname |
      "! @parameter rv_present |
      is_annotation_present_by_name ABSTRACT IMPORTING iv_classname      TYPE abap_classname
                                             RETURNING VALUE(rv_present) TYPE abap_bool,
      "!
      "! @parameter io_descr |
      "! @parameter rv_present |
      is_annotation_present_by_descr ABSTRACT IMPORTING io_descr          TYPE REF TO cl_abap_classdescr
                                              RETURNING VALUE(rv_present) TYPE abap_bool,
      "!
      "! @parameter iv_annotation_key |
      "! @parameter ro_annotation |
      "! @raising zcx_aap_illegal_argument |
      get_annotation_by_name ABSTRACT IMPORTING iv_classname         TYPE abap_classname
                                      RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                                      RAISING   zcx_aap_illegal_argument,
      "!
      "! @parameter io_descr |
      "! @parameter ro_annotation |
      "! @raising zcx_aap_illegal_argument |
      get_annotation_by_descr ABSTRACT IMPORTING io_descr             TYPE REF TO cl_abap_classdescr
                                       RETURNING VALUE(ro_annotation) TYPE REF TO zcl_aap_annotation_base
                                       RAISING   zcx_aap_illegal_argument,
      "!
      "! @parameter rt_annotations |
      get_annotations ABSTRACT RETURNING VALUE(rt_annotations) TYPE zif_aap_annotation_resolver=>gty_annotation_tab,
      "! Force populate internal caches recursively (disables loading on demand)
      load_all ABSTRACT.
  PROTECTED SECTION.
    CONSTANTS:
      gc_annotatable_intf_name TYPE abap_intfname VALUE 'ZIF_AAP_ANNOTATABLE'.
    METHODS:
      constructor.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_proc_base IMPLEMENTATION.
  METHOD constructor ##NEEDED.
  ENDMETHOD.
ENDCLASS.