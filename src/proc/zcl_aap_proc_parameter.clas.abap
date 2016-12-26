"! Parameter processor
CLASS zcl_aap_proc_parameter DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_aap_proc_base
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_aap_proc_method.

  PUBLIC SECTION.
    METHODS:
      load_all REDEFINITION,
      get_annotations REDEFINITION.
    DATA:
      "! Parameter description
      ms_parameter_description  TYPE abap_parmdescr READ-ONLY,
      "! Name of the containing class or interface
      mv_containing_object_name TYPE abap_classname,
      "! Name of the containing method
      mv_containing_method_name TYPE abap_methname,
      "! Name of the parameter in its containing method
      mv_parameter_name         TYPE abap_parmname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING is_parameter_description  TYPE abap_parmdescr
                            iv_containing_object_name TYPE abap_classname
                            iv_containing_method_name TYPE abap_methname.
ENDCLASS.



CLASS zcl_aap_proc_parameter IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    ASSERT: is_parameter_description IS NOT INITIAL,
            iv_containing_method_name IS NOT INITIAL,
            iv_containing_object_name IS NOT INITIAL.

    ms_parameter_description = is_parameter_description.
    mv_containing_method_name = iv_containing_method_name.
    mv_containing_object_name = iv_containing_object_name.
    mv_parameter_name = is_parameter_description-name.
  ENDMETHOD.

  METHOD load_all ##NEEDED.
    " Nothing to load here
  ENDMETHOD.

  METHOD get_annotations.
    " TODO: Think of propagating the exception in some way
    rt_annotations = get_resolver( )->get_annotations_for_parameter(
                         iv_containing_object_name = mv_containing_object_name
                         iv_containing_method_name = mv_containing_method_name
                         iv_parameter_name         = mv_parameter_name
                     ).
  ENDMETHOD.
ENDCLASS.