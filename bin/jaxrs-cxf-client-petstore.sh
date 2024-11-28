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
ags="$@ generate --artifact-id "jaxrs-cxf-petstore-client" -i modules/swagger-codegen/src/test/resources/3_0_0/petstore.yaml -l jaxrs-cxf-client -o samples/client/petstore/jaxrs-cxf-client/"

echo "Removing files and folders under samples/client/petstore/jaxrs-cxf-client/src/main"
rm -rf samples/client/petstore/jaxrs-cxf-client/src/main
rm -rf samples/client/petstore/jaxrs-cxf-client/src/gen
find samples/client/petstore/jaxrs-cxf-client -maxdepth 1 -type f ! -name "README.md" -exec rm {} +

java $JAVA_OPTS -jar $executable $ags
