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

rm -rf dists/purecloud/csharp
mkdir -p dists/purecloud/csharp/src/ININ.PureCloudApi/Extensions


cp -R /git/PureCloudApiLibrary_CSharp/Extensions dists/purecloud/csharp/src/ININ.PureCloudApi/Extensions

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="$@ generate -i https://api.mypurecloud.com/api/v2/docs/swagger -l purecloudcsharp -o dists/purecloud/csharp -c bin/config/purecloud-csharp.json -t /git/PureCloudApiLibrary_CSharp/swagger_template"

java $JAVA_OPTS -jar $executable $ags
