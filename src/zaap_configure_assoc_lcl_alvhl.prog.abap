*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_ALVHL.
*----------------------------------------------------------------------*

" Whatever it takes to get drag and drop working on SALV tree (please don't judge me)
CLASS lcl_salv_backdoor DEFINITION
  INHERITING FROM cl_salv_controller
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_alv_from_salv IMPORTING io_salv       TYPE REF TO cl_salv_tree
                        RETURNING VALUE(ro_alv) TYPE REF TO cl_salv_gui_tree.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_salv_backdoor IMPLEMENTATION.
  METHOD get_alv_from_salv.
    " Friendship is rare, do you know what I'm saying to you, friendship is rare...
    ro_alv = CAST cl_salv_tree_adapter_base( io_salv->r_controller->r_adapter )->r_tree.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_alv_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      add_drag_drop_to_node IMPORTING iv_node_key   TYPE lvc_nkey
                                      iv_dragdropid TYPE i
                                      io_alv        TYPE REF TO cl_salv_gui_tree.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_alv_helper IMPLEMENTATION.
  METHOD add_drag_drop_to_node.
    DATA: lv_dummy TYPE i.

    io_alv->change_node(
      EXPORTING
        i_node_key     = iv_node_key
        i_outtab_line  = lv_dummy
        is_node_layout = VALUE #( dragdropid = iv_dragdropid u_dragdrop = abap_true )
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
  ENDMETHOD.
ENDCLASS.
