*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_BUSTL.
*----------------------------------------------------------------------*

INTERFACE lif_dynpro_binding.
  CLASS-DATA:
    gv_dynnr TYPE dynnr.
ENDINTERFACE.

CLASS lcl_bus_tools DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_screen IMPORTING iv_program       TYPE progname DEFAULT sy-cprog
                           iv_dynnr         TYPE dynnr DEFAULT sy-dynnr
                             PREFERRED PARAMETER iv_dynnr
                 RETURNING VALUE(ro_screen) TYPE REF TO cl_bus_abstract_screen
                 RAISING   lcx_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_bus_tools IMPLEMENTATION.
  METHOD get_screen.
    CONSTANTS: lc_dummy_screen TYPE abap_abstypename
                               VALUE '\CLASS-POOL=CL_BUS_ABSTRACT_SCREEN\CLASS=LCL_SCREEN_DUMMY'.

    cl_bus_abstract_screen=>get_screen(
         EXPORTING
           iv_program_name  = iv_program
           iv_dynpro_number = iv_dynnr
         IMPORTING
           ev_screen = ro_screen
    ).

    " Is always be the case because the framework returns a dummy instance
    ASSERT ro_screen IS BOUND.

    " Is it the dummy screen?
    DATA(lo_descr) = CAST cl_abap_objectdescr(
                       cl_abap_typedescr=>describe_by_object_ref( ro_screen )
                     ).
    IF lo_descr->absolute_name = lc_dummy_screen.
      lcx_error=>raise( |Screen { iv_dynnr } not found in program { iv_program }.| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
