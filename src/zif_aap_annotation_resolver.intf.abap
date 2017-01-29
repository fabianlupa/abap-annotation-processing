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
    "! Retrieve all annotations for a class / an interface
    "! @parameter iv_name | Class / interface name
    "! @parameter rt_annotations | Found annotations
    "! @raising zcx_aap_incons_customizing | Customizing is not consistent with annotation objects
    "! @raising zcx_aap_illegal_argument | Class not found
    get_annotations_for_object IMPORTING iv_name               TYPE abap_classname
                               RETURNING VALUE(rt_annotations) TYPE gty_annotation_tab
                               RAISING   zcx_aap_incons_customizing
                                         zcx_aap_illegal_argument,
    "! Retrieve all annotations for an attribute
    "! @parameter iv_containing_object_name | Containing class or interface name
    "! @parameter iv_attribute_name | Attribute name
    "! @parameter rt_annotations | Found annotations
    "! @raising zcx_aap_incons_customizing | Customizing is not consistent with annotation objects
    "! @raising zcx_aap_illegal_argument | Attribute / class not found
    get_annotations_for_attribute IMPORTING iv_containing_object_name TYPE abap_classname
                                            iv_attribute_name         TYPE abap_attrname
                                  RETURNING VALUE(rt_annotations)     TYPE gty_annotation_tab
                                  RAISING   zcx_aap_incons_customizing
                                            zcx_aap_illegal_argument,
    "! Retrieve all annotations for a method
    "! @parameter iv_containing_object_name | Containing class or interface name
    "! @parameter iv_method_name | Name of the method
    "! @parameter rt_annotations | Found annotations
    "! @raising zcx_aap_incons_customizing | Customizing is not consistent with annotation objects
    "! @raising zcx_aap_illegal_argument | Method / class not found
    get_annotations_for_method IMPORTING iv_containing_object_name TYPE abap_classname
                                         iv_method_name            TYPE abap_methname
                               RETURNING VALUE(rt_annotations)     TYPE gty_annotation_tab
                               RAISING   zcx_aap_incons_customizing
                                         zcx_aap_illegal_argument,
    "! Retrieve all annotations for a parameter of a method
    "! @parameter iv_containing_object_name | Containing class or interface name
    "! @parameter iv_containing_method_name | Containing method name
    "! @parameter iv_parameter_name | Parameter name
    "! @parameter rt_annotations | Found annotations
    "! @raising zcx_aap_incons_customizing | Customizing is not consistent with annotation objects
    "! @raising zcx_aap_illegal_argument | Parameter / method / class not found
    get_annotations_for_parameter IMPORTING iv_containing_object_name TYPE abap_classname
                                            iv_containing_method_name TYPE abap_methname
                                            iv_parameter_name         TYPE abap_parmname
                                  RETURNING VALUE(rt_annotations)     TYPE gty_annotation_tab
                                  RAISING   zcx_aap_incons_customizing
                                            zcx_aap_illegal_argument.
ENDINTERFACE.
