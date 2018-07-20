#!/bin/bash

# grep for \r in the java classes
find src/main/java/ -type f -iname "*.java" -exec grep -RUIl $'\r$' {} \;find modules/swagger-codegen/src/main/java/io/swagger/codegen/ -type f -iname "*.java" -exec grep -RUIl $'\r$' {} \; | wc -l
if [ $? -ne 0 ]; then
    echo "Java classes contain carriage return '/r'. Please remove it and try again."
    exit 1;
fi
