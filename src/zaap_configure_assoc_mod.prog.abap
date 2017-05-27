*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_MOD.
*----------------------------------------------------------------------*

MODULE dynpro_pbo OUTPUT.
  cl_bus_abstract_screen=>dynpro_pbo( iv_dynpro_number = sy-dynnr iv_program_name = sy-repid ).
ENDMODULE.

MODULE dynpro_pai INPUT.
  cl_bus_abstract_screen=>dynpro_pai( iv_dynpro_number = sy-dynnr iv_program_name = sy-repid ).
ENDMODULE.
