"! Dependency switcher for test scenarios
CLASS ztcl_aap_test_dep_switcher DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Switch default resolver for testing
      "! @parameter ii_resolver | New default resolver
      switch_default_resolver IMPORTING ii_resolver TYPE REF TO zif_aap_annotation_resolver,
      "! Switch default changer for testing
      "! @parameter ii_changer | New default changer
      switch_default_changer IMPORTING ii_changer TYPE REF TO zif_aap_annotation_changer.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ztcl_aap_test_dep_switcher IMPLEMENTATION.
  METHOD switch_default_resolver.
    ASSERT ii_resolver IS BOUND.
    zcl_aap_dependency_injector=>gi_resolver = ii_resolver.
  ENDMETHOD.

  METHOD switch_default_changer.
    ASSERT ii_changer IS BOUND.
    zcl_aap_dependency_injector=>gi_changer = ii_changer.
  ENDMETHOD.
ENDCLASS.
