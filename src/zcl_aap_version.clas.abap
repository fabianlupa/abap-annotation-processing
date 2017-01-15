"! Version constants for AAP
CLASS zcl_aap_version DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      "! Version of AAP library in major.minor.revision format
      gc_version TYPE string VALUE '0.1.0-SNAPSHOT'.
    CLASS-DATA:
      mv_major       TYPE i READ-ONLY,
      mv_minor       TYPE i READ-ONLY,
      mv_revision    TYPE i READ-ONLY,
      mv_is_snapshot TYPE abap_bool READ-ONLY.
    CLASS-METHODS:
      class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aap_version IMPLEMENTATION.
  METHOD class_constructor.
    SPLIT gc_version AT '-' INTO TABLE DATA(lt_split1).

    IF lines( lt_split1 ) = 2.
      mv_is_snapshot = abap_true.
    ENDIF.

    SPLIT lt_split1[ 1 ] AT '.' INTO TABLE DATA(lt_split2).

    mv_major = lt_split2[ 1 ].
    mv_minor = lt_split2[ 2 ].
    mv_revision = lt_split2[ 3 ].
  ENDMETHOD.
ENDCLASS.