#!/bin/sh

## Get the tests from the anonymous CVS at
##    http://dev.w3.org/cvsweb/2001/DOM-Test-Suite/

dir="2001/DOM-Test-Suite/tests/level1/core/"
test="$1"

gnatmake -q -m -P conformance.gpr dom_conformance
if [ $? != 0 ]; then
   exit 1
fi

./dom_conformance "$dir" $test

