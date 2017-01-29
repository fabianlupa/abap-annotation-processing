"! Dependency injector
CLASS zcl_aap_dependency_injector DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS ztcl_aap_test_dep_switcher.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      "! Get the configured resolver instance
      "! @parameter ri_resolver | Resolver
      get_resolver RETURNING VALUE(ri_resolver) TYPE REF TO zif_aap_annotation_resolver,
      "! Get the configured changer instance
      "! @parameter ri_changer | Changer
      get_changer RETURNING VALUE(ri_changer) TYPE REF TO zif_aap_annotation_changer.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_resolver TYPE REF TO zif_aap_annotation_resolver,
      gi_changer  TYPE REF TO zif_aap_annotation_changer.
ENDCLASS.



CLASS zcl_aap_dependency_injector IMPLEMENTATION.
  METHOD class_constructor.
    gi_resolver = NEW zcl_aap_pers_db_resolver( ).
    gi_changer = NEW zcl_aap_pers_db_changer( ).
  ENDMETHOD.

  METHOD get_resolver.
    ri_resolver = gi_resolver.
  ENDMETHOD.

  METHOD get_changer.
    ri_changer = gi_changer.
  ENDMETHOD.
ENDCLASS.
