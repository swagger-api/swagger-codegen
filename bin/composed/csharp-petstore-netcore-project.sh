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

if [ ! -d "${APP_DIR}" ]; then
  APP_DIR=`dirname "$SCRIPT"`/..
  APP_DIR=`cd "${APP_DIR}"; pwd`
fi

executable="./modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -Dlogback.configurationFile=bin/logback.xml"
ags="generate $@ -i modules/swagger-codegen/src/test/resources/3_0_0/petstore-with-composed-schemas.yaml -l csharp -o samples/composed/client/petstore/csharp/SwaggerClientNetCoreProject --additional-properties packageGuid={67035b31-f8e5-41a4-9673-954035084f7d},netCoreProjectFile=true -c bin/csharp-petstore-net-standard.json"

# java $JAVA_OPTS -jar $executable $ags
