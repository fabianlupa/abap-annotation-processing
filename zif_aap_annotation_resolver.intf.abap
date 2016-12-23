INTERFACE zif_aap_annotation_resolver PUBLIC.
  TYPES:
    BEGIN OF gty_annotation,
      classname  TYPE abap_classname,
      descriptor TYPE REF TO cl_abap_classdescr,
      instance   TYPE REF TO zcl_aap_annotation_base,
    END OF gty_annotation,
    gty_annotation_tab TYPE SORTED TABLE OF gty_annotation
                            WITH UNIQUE KEY classname.
  METHODS:
    "!
    "! @parameter iv_name |
    "! @parameter rt_annotations |
    get_annotations_for_object IMPORTING iv_name               TYPE abap_classname
                               RETURNING VALUE(rt_annotations) TYPE gty_annotation_tab,
    "!
    "! @parameter iv_containing_obj_name |
    "! @parameter iv_attribute_name |
    "! @parameter rt_annotations |
    get_annotations_for_attribute IMPORTING iv_containing_object_name TYPE abap_classname
                                            iv_attribute_name         TYPE abap_attrname
                                  RETURNING VALUE(rt_annotations)     TYPE gty_annotation_tab,
    "!
    "! @parameter iv_containing_obj_name |
    "! @parameter iv_method_name |
    "! @parameter rt_annotations |
    get_annotations_for_method    IMPORTING iv_containing_object_name TYPE abap_classname
                                            iv_method_name            TYPE abap_methname
                                  RETURNING VALUE(rt_annotations)     TYPE gty_annotation_tab,
    "!
    "! @parameter iv_containing_obj_name |
    "! @parameter iv_containing_meth_name |
    "! @parameter iv_parameter_name |
    "! @parameter rt_annotations |
    get_annotations_for_parameter IMPORTING iv_containing_object_name TYPE abap_classname
                                            iv_containing_method_name TYPE abap_methname
                                            iv_parameter_name         TYPE abap_parmname
                                  RETURNING VALUE(rt_annotations)     TYPE gty_annotation_tab.
ENDINTERFACE.