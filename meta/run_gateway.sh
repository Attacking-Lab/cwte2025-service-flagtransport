#!/bin/bash
# -x => executable
# -j => run
# -Wall -Wextra => If you thought that C/C++ is bad with warnings, then your soul will perish from this world after this.
#       Even hello world produces a warning! Taken from GnuCOBOL manual, 5 lines of code, 2 lines of actual "instructions"...
# -Wno-terminator => No warnings about not using END-* verbs
# -Wno-possible-truncate => No warning about vulnerability (ignored code at end of the line)
# -Wno-additional => No warning about implicit size for filler values
cobc -xj -Wall -Wextra -Wno-terminator -Wno-possible-truncate -Wno-additional service/src/gateway.cob
