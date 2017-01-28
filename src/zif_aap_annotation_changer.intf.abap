"! Annotation changer
"! <p>
"! Allows for adding annotations to objectes / methods / attributes / parameters or remove / change
"! them. Parameter values can also be modified.
"! </p>
"! <p>
"! Implementations of this interface should not be used in productive systems, this is for
"! customizing only.
"! </p>
INTERFACE zif_aap_annotation_changer PUBLIC.
  TYPES:
    BEGIN OF gty_parameter,
      name  TYPE abap_attrname,
      value TYPE string,
    END OF gty_parameter,
    gty_parameter_tab TYPE HASHED TABLE OF gty_parameter WITH UNIQUE KEY name.
  METHODS:
    "! Add an annotation to a class or interface
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_annotationname | Name of the to be added annotation
    "! @parameter it_parameters | Annotation parameters
    "! @raising zcx_aap_annot_change_failure | Adding the annotation failed
    add_to_object IMPORTING iv_objectname     TYPE abap_classname
                            iv_annotationname TYPE abap_classname
                            it_parameters     TYPE gty_parameter_tab OPTIONAL
                  RAISING   zcx_aap_annot_change_failure,
    "! Add an annotation to an attribute
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_attributename | Name of the to be added annotation
    "! @parameter iv_annotationname | Name of the attribute
    "! @parameter it_parameters | Annotation parameters
    "! @raising zcx_aap_annot_change_failure | Adding the annotation failed
    add_to_attribute IMPORTING iv_objectname     TYPE abap_classname
                               iv_attributename  TYPE abap_attrname
                               iv_annotationname TYPE abap_classname
                               it_parameters     TYPE gty_parameter_tab OPTIONAL
                     RAISING   zcx_aap_annot_change_failure,
    "! Add an annotation to a method
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_methodname | Name of the method
    "! @parameter iv_annotationname | Name of the to be added annotation
    "! @parameter it_parameters | Annotation parameters
    "! @raising zcx_aap_annot_change_failure | Adding the annotation failed
    add_to_method IMPORTING iv_objectname     TYPE abap_classname
                            iv_methodname     TYPE abap_methname
                            iv_annotationname TYPE abap_classname
                            it_parameters     TYPE gty_parameter_tab OPTIONAL
                  RAISING   zcx_aap_annot_change_failure,
    "! Add an annotation to a parameter
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_methodname | Name of the method
    "! @parameter iv_parametername | Name of the parameter
    "! @parameter iv_annotationname | Name of the to be added annotation
    "! @parameter it_parameters | Annotation parameters
    "! @raising zcx_aap_annot_change_failure | Adding the annotation failed
    add_to_parameter IMPORTING iv_objectname     TYPE abap_classname
                               iv_methodname     TYPE abap_methname
                               iv_parametername  TYPE abap_parmname
                               iv_annotationname TYPE abap_classname
                               it_parameters     TYPE gty_parameter_tab OPTIONAL
                     RAISING   zcx_aap_annot_change_failure,
    "! Remove an annotation from a class / interface
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_annotationname | Name of the to be removed annotation
    "! @raising zcx_aap_annot_change_failure | Removing the annotation failed
    remove_from_object IMPORTING iv_objectname     TYPE abap_classname
                                 iv_annotationname TYPE abap_classname
                       RAISING   zcx_aap_annot_change_failure,
    "! Remove an annotation from an attribute
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_attributename | Name of the attribute
    "! @parameter iv_annotationname | Name of the to be removed annotation
    "! @raising zcx_aap_annot_change_failure | Removing the annotation failed
    remove_from_attribute IMPORTING iv_objectname     TYPE abap_classname
                                    iv_attributename  TYPE abap_attrname
                                    iv_annotationname TYPE abap_classname
                          RAISING   zcx_aap_annot_change_failure,
    "! Remove an annotation from a method
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_methodname | Name of the method
    "! @parameter iv_annotationname | Name of the to be removed annotation
    "! @raising zcx_aap_annot_change_failure | Removing the annotation failed
    remove_from_method IMPORTING iv_objectname     TYPE abap_classname
                                 iv_methodname     TYPE abap_methname
                                 iv_annotationname TYPE abap_classname
                       RAISING   zcx_aap_annot_change_failure,
    "! Remove an annotation from a parameter
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_methodname | Name of the method
    "! @parameter iv_parametername | Name of the parameter
    "! @parameter iv_annotationname | Name of the to be removed annotation
    "! @raising zcx_aap_annot_change_failure | Removing the annotation failed
    remove_from_parameter IMPORTING iv_objectname     TYPE abap_classname
                                    iv_methodname     TYPE abap_methname
                                    iv_parametername  TYPE abap_parmname
                                    iv_annotationname TYPE abap_classname
                          RAISING   zcx_aap_annot_change_failure,
    "! Change parameters for an annotation of a class / interface
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_annotationname | Name of the annotation to be changed
    "! @parameter it_parameters | New annotation parameters
    "! @raising zcx_aap_annot_change_failure | Changing the annotation failed
    change_for_object IMPORTING iv_objectname     TYPE abap_classname
                                iv_annotationname TYPE abap_classname
                                it_parameters     TYPE gty_parameter_tab OPTIONAL
                      RAISING   zcx_aap_annot_change_failure,
    "! Change parameters for an annotation of an attribute
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_attributename | Name of the attribute
    "! @parameter iv_annotationname | Name of the annotation to be changed
    "! @parameter it_parameters | New annotation parameters
    "! @raising zcx_aap_annot_change_failure | Changing the annotation failed
    change_for_attribute IMPORTING iv_objectname     TYPE abap_classname
                                   iv_attributename  TYPE abap_attrname
                                   iv_annotationname TYPE abap_classname
                                   it_parameters     TYPE gty_parameter_tab OPTIONAL
                         RAISING   zcx_aap_annot_change_failure,
    "! Change parameters for an annotation of a method
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_methodname | Name of the method
    "! @parameter iv_annotationname | Name of the annotation to be changed
    "! @parameter it_parameters | New annotation parameters
    "! @raising zcx_aap_annot_change_failure | Changing the annotation failed
    change_for_method IMPORTING iv_objectname     TYPE abap_classname
                                iv_methodname     TYPE abap_methname
                                iv_annotationname TYPE abap_classname
                                it_parameters     TYPE gty_parameter_tab OPTIONAL
                      RAISING   zcx_aap_annot_change_failure,
    "! Change parameters for an annotation of a parameter
    "! @parameter iv_objectname | Class / interface name which implements ZIF_AAP_ANNOTATABLE
    "! @parameter iv_methodname | Name of the method
    "! @parameter iv_parametername | Name of the parameter
    "! @parameter iv_annotationname | Name of the annotation to be changed
    "! @parameter it_parameters | New annotation parameters
    "! @raising zcx_aap_annot_change_failure | Changing the annotation failed
    change_for_parameter IMPORTING iv_objectname     TYPE abap_classname
                                   iv_methodname     TYPE abap_methname
                                   iv_parametername  TYPE abap_parmname
                                   iv_annotationname TYPE abap_classname
                                   it_parameters     TYPE gty_parameter_tab OPTIONAL
                         RAISING   zcx_aap_annot_change_failure.
ENDINTERFACE.