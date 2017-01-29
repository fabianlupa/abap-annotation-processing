REPORT zaap_example01 NO STANDARD PAGE HEADING.

DATA(go_annotated_class) = NEW zcl_aap_example_class( ).

" Check if class has annotation
DATA(go_processor) = zcl_aap_proc_object=>from_object( go_annotated_class ).
IF go_processor->is_annotation_present_by_name( 'ZCL_AAP_EXAMPLE_ANNOTATION' ) = abap_true.
  WRITE 'Annotation is there'.
ELSE.
  WRITE 'Annotation is not there'.
ENDIF.
