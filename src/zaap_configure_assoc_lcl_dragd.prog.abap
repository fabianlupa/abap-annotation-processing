*----------------------------------------------------------------------*
***INCLUDE ZAAP_CONFIGURE_ASSOC_LCL_DRAGD.
*----------------------------------------------------------------------*

CLASS lcl_annotation_drop DEFINITION.
  PUBLIC SECTION.
    TYPES:
      gty_annotation_tab TYPE STANDARD TABLE OF abap_classname.
    METHODS:
      constructor IMPORTING it_annotations TYPE gty_annotation_tab.
    DATA:
      mt_annotations TYPE gty_annotation_tab READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_annotation_drop IMPLEMENTATION.
  METHOD constructor.
    mt_annotations = it_annotations.
  ENDMETHOD.
ENDCLASS.
