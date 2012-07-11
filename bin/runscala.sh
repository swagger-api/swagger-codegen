#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORDNIK_OPTS=${WORDNIK_OPTS:=NOPE}

if [ $WORDNIK_OPTS = NOPE ];
	then
    export WORDNIK_OPTS="-DconfigFile=conf/config.xml -DdevMode=true"
	
fi

export CLASSPATH="$DIR/../target/lib/*:$DIR/../target/*"
export JAVA_OPTS="${JAVA_OPTS} -DrulePath=data -Xmx4096M"
scala $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS -cp $CLASSPATH "$@"
