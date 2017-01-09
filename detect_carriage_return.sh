#!/bin/bash

# grep for \r in the template
grep -RUIl $'\r$' modules/swagger-codegen/src/main/resources/*

if [ $? -ne 1 ]; then
    echo "Templates contain carriage return. Please remove it and try again."
    exit
fi


