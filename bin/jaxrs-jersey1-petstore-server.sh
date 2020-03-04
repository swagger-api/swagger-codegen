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
ags="$@ generate -i modules/swagger-codegen/src/test/resources/3_0_0/petstore-with-fake-endpoints-models-for-testing.yaml -l jaxrs-jersey -o samples/server/petstore/jaxrs/jersey1 -DhideGenerationTimestamp=true,serverPort=8080 --library=jersey1 --artifact-id=swagger-jaxrs-jersey1-server"

echo "Removing files and folders under samples/server/petstore/jaxrs/jersey1/src/main"
rm -rf samples/server/petstore/jaxrs/jersey1/src/main
rm -rf samples/server/petstore/jaxrs/jersey1/src/gen
find samples/server/petstore/jaxrs/jersey1 -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags
