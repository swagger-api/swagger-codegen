#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export SCALA_HOME=/Users/jpk/dev/scala-2.9.1.final
export PATH=$SCALA_HOME/bin:$PATH

export CLASSPATH="$DIR/../target/*:$DIR/../target/lib/*"
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
JAVA_OPTS=$JAVA_OPTS scala -cp $CLASSPATH "$@" samples/client/xapp-server/objc/ObjcXappServerCodegen.scala http://localhost:9000/api-docs.json special-key
