#!/bin/sh

SCRIPT="$0"

while [ -h "$SCRIPT" ] ; do
  ls=`ls -ld "$SCRIPT"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    SCRIPT="$link"
  else
    SCRIPT=`dirname "$SCRIPT"`/"$link"
  fi
done


executable="swagger-codegen-cli.jar"

LANG=$1

JOB_NAME=$2
if [ -z "$JOB_NAME" ]
then
      JOB_NAME=$LANG
fi

SPEC_URL=$3
if [[ $SPEC_URL == "null" ]];
then
      SPEC_URL="https://petstore3.swagger.io/api/v3/openapi.json"
fi

shift;
shift;
shift;

export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -Dlogback.configurationFile=$SCRIPT/logback.xml"
ags="generate -i ${SPEC_URL} -l ${LANG} -o generated/${JOB_NAME} $@"

java $JAVA_OPTS -jar $executable $ags


