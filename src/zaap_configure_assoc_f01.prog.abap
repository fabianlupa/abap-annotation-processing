*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_F01.
*----------------------------------------------------------------------*

FORM bus_screen_create USING VALUE(pv_program_name)  TYPE bus_screen-program_name
                             VALUE(pv_dynpro_number) TYPE bus_screen-dynpro_number
                       CHANGING co_screen TYPE REF TO cl_bus_abstract_screen ##CALLED.
  CASE pv_dynpro_number.
    WHEN '0100'.
      CREATE OBJECT co_screen TYPE lcl_main_screen
        EXPORTING
          iv_program_name  = pv_program_name
          iv_dynpro_number = pv_dynpro_number.
    WHEN '0101'.
      CREATE OBJECT co_screen TYPE lcl_annotation_pick_screen
        EXPORTING
          iv_program_name  = pv_program_name
          iv_dynpro_number = pv_dynpro_number.
  ENDCASE.
ENDFORM.
