# AAP - ABAP Annotation Processing [![Build Status](https://travis-ci.com/flaiker/abap-annotation-processing.svg?token=dqh3yJEMxtgMhb4syMRh&branch=master)](https://travis-ci.com/flaiker/abap-annotation-processing)
Annotations in ABAP!

## Using annotations in ABAP code
```abap
DATA: lo_annotation TYPE REF TO zcl_my_annotation.

DATA(lo_annotated_class) = NEW zcl_annotated_class( ).

DATA(lo_processor) = zcl_aap_proc_object=>from_object( lo_annotated_class ).
IF lo_processor->is_annotation_present_by_data( lo_annotation ).
  lo_annotation ?= lo_processor->get_annotation_by_data( lo_annotation ).
  WRITE lo_annotation->mv_my_attribute.
ENDIF.
```
[More example code](https://github.com/flaiker/abap-annotation-processing/tree/master/src/example)

## Adding annotations to classes/interfaces/attributes/methods/parameters
Using transaction **ZAAP_CUST**:
![Transaction ZAAP_CUST](https://github.com/flaiker/abap-annotation-processing/wiki/rendered/zaap_cust.PNG)
IDE support for the ABAP Development Tools (Eclipse) is planned.

## API Overview
![API Overview](https://github.com/flaiker/abap-annotation-processing/wiki/rendered/api-overview.png)
[Detailed class diagram](https://github.com/flaiker/abap-annotation-processing/wiki/rendered/api-detail.png)

## License
[MIT License Copyright (c) 2017 Fabian Lupa](LICENSE)
