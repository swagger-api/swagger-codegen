#!/bin/bash
if [ $# -ne 4 ]
then
    echo "Error in $0 - Invalid Argument Count"
    echo "Syntax: $0 location_of_service api_key package_name library_root"
    exit
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export CLASSPATH="$DIR/../target/lib/*:$DIR/../target/*"
export JAVA_OPTS="${JAVA_OPTS} -Dproperty=Xmx2g"
java $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS $JAVA_OPTS -cp $CLASSPATH com.wordnik.swagger.codegen.config.python.PythonLibCodeGen "$@"