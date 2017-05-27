*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_CONST.
*----------------------------------------------------------------------*

"! Common constants and types
CLASS lcl_const DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      gty_mode TYPE c LENGTH 1.
    CONSTANTS:
      gc_mode_view TYPE gty_mode VALUE 'V',
      gc_mode_edit TYPE gty_mode VALUE 'E'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
