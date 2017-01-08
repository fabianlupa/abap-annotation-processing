REPORT zaap_example02 NO STANDARD PAGE HEADING.

PARAMETERS: p_modr TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND u1,
            p_modd TYPE abap_bool RADIOBUTTON GROUP r1,
            p_modn TYPE abap_bool RADIOBUTTON GROUP r1.
PARAMETERS: p_clsnam TYPE abap_classname DEFAULT 'ZCL_AAP_EXAMPLE_CLASS'.

"! Write given annotation table to list output
"! @parameter pt_annotations | Annotations to write
FORM write_annotations USING pt_annotations TYPE zif_aap_annotation_resolver=>gty_annotation_tab.
  LOOP AT pt_annotations ASSIGNING FIELD-SYMBOL(<ls_cls_annotation>).
    WRITE: / <ls_cls_annotation>-classname.

    LOOP AT <ls_cls_annotation>-descriptor->attributes ASSIGNING FIELD-SYMBOL(<ls_attr>)
        WHERE visibility = cl_abap_objectdescr=>public.

      ASSIGN <ls_cls_annotation>-instance->(<ls_attr>-name) TO FIELD-SYMBOL(<lg_attr>).
      WRITE: / |{ <ls_attr>-name }: '{ <lg_attr> }'|.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_CLSNAM'.
      screen-input = COND #( WHEN p_modn = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  DATA(go_ref) = NEW zcl_aap_example_class( ).

  TRY.
      DATA(go_proc) = COND #( WHEN p_modr = abap_true
                              THEN zcl_aap_proc_object=>from_object( go_ref )
                              WHEN p_modn = abap_true
                              THEN zcl_aap_proc_object=>from_name( p_clsnam )
                              WHEN p_modd = abap_true
                              THEN zcl_aap_proc_object=>from_descriptor(
                                     CAST #( cl_abap_typedescr=>describe_by_object_ref(
                                               go_ref
                                             )
                                   )
                              )
      ).
    CATCH zcx_aap_illegal_argument INTO DATA(gx_ex).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  WRITE: / |Analyzing class { go_proc->mv_classname_relative }|.
  ULINE.
  ULINE.
  SKIP.

  WRITE: / |Class level annotations:|.
  ULINE.

  DATA(gt_cls_annotations) = go_proc->get_annotations( ).
  PERFORM write_annotations USING gt_cls_annotations.

  SKIP.

  WRITE: / |Attribute level annotations:|.
  ULINE.

  LOOP AT go_proc->get_attribute_processors( ) ASSIGNING FIELD-SYMBOL(<gs_attr_proc>).
    WRITE: / |{ <gs_attr_proc>-attribute_name }|.
    DATA(gt_attr_annotations) = <gs_attr_proc>-processor->get_annotations( ).
    PERFORM write_annotations USING gt_attr_annotations.
    SKIP.
  ENDLOOP.

  SKIP.

  WRITE: / |Method and parameter level annotations:|.
  ULINE.

  DATA(gt_mthpar_annotations) = go_proc->get_annotations( ).
  PERFORM write_annotations USING gt_mthpar_annotations.