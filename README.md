# AAP - ABAP Annotation Processing [![Build Status](https://travis-ci.org/flaiker/abap-annotation-processing.svg?branch=master)](https://travis-ci.org/flaiker/abap-annotation-processing) [![ABAP Doc](https://img.shields.io/badge/ABAP%20Doc-latest-blue.svg)](https://flaiker.github.io/abap-annotation-processing/) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
Annotations in ABAP!

**This project is a technology study / proof of concept of implementing a framework to manage class based annotations in ABAP. Since there is no native language support for annotations the assignments are done in customizing tables. The current state is still very much unfinished and not production ready. I am currently not planning on finishing this project (the necessary efforts and drawbacks are way too high). Feel free to fork it or take parts of it for reference.**

**DO NOT USE IN PRODUCTION.**

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

## API Overview
![API Overview](https://github.com/flaiker/abap-annotation-processing/wiki/rendered/api-overview.png)
[Detailed class diagram](https://github.com/flaiker/abap-annotation-processing/wiki/rendered/api-detail.png)

## License
[MIT License Copyright (c) 2017 Fabian Lupa](LICENSE)
