*&--------------------------------------------------------------------*
*& Program               ZAAP_DEBUG_INFO
*& Author                Fabian Lupa
*& Short description     Show debug information
*&--------------------------------------------------------------------*
*& Date                  2017-01-09
*& Last change           2016-01-09
*&--------------------------------------------------------------------*
*& Change protocol
*& Date       User         Description
*& ____-__-__ ____________ ______________________________________
*&--------------------------------------------------------------------*

PROGRAM zaap_debug_info.

cl_demo_output=>new(
  )->begin_section( 'AAP Abap Annotation Processing Debug Information'
  )->write_data( name = 'Version' value = zcl_aap_version=>gc_version
  )->display( ).

LEAVE PROGRAM.