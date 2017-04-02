#!/bin/bash

# grep for \t in the generators
RESULT=`find modules/swagger-codegen/src/ -name "*.java" | xargs grep "\t"`

if [ "$RESULT" != "" ]; then
    echo "Java files contain tab '\\t'. Please remove it and try again."
    echo $RESULT
    exit 1;
fi

