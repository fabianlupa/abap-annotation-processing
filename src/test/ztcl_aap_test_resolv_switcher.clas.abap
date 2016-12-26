"! Resolver switcher for test scenarios
CLASS ztcl_aap_test_resolv_switcher DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Switch default resolver for testing
      "! @parameter ii_resolver | New default resolver
      switch_default_resolver IMPORTING ii_resolver TYPE REF TO zif_aap_annotation_resolver.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ztcl_aap_test_resolv_switcher IMPLEMENTATION.
  METHOD switch_default_resolver.
    zcl_aap_resolver_injector=>gi_resolver = ii_resolver.
  ENDMETHOD.
ENDCLASS.