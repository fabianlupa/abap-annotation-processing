*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_VIEWB.
*----------------------------------------------------------------------*

CLASS lcl_view_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_container TYPE REF TO cl_gui_container,
      display.
  PROTECTED SECTION.
    DATA:
      mo_container TYPE REF TO cl_gui_container.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_view_base IMPLEMENTATION.
  METHOD constructor.
    mo_container = io_container.
  ENDMETHOD.

  METHOD display ##NEEDED.
  ENDMETHOD.
ENDCLASS.
