"! Resolver injector
CLASS zcl_aap_resolver_injector DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS ztcl_aap_test_resolv_switcher.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      get_resolver RETURNING VALUE(ri_resolver) TYPE REF TO zif_aap_annotation_resolver.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_resolver TYPE REF TO zif_aap_annotation_resolver.
ENDCLASS.



CLASS zcl_aap_resolver_injector IMPLEMENTATION.
  METHOD class_constructor.
    gi_resolver = NEW zcl_aap_db_annotation_resolver( ).
  ENDMETHOD.

  METHOD get_resolver.
    ri_resolver = gi_resolver.
  ENDMETHOD.
ENDCLASS.