if $Build_Codegen ; then
    mvn3 clean package -Dmaven.test.skip=true
fi

echo "{\"artifactVersion\":\"${Version}\"}">config.json
echo "GENERATING SDK"
if [ "$Client" = "java" ]
then
  rm -rf promotion_engine/java_client/java
  mkdir -p promotion_engine/java_client/java
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i promotion-engine.json  \
  -l java \
  -DdateLibrary=java11 \
  -o promotion_engine/java_client/java \
  -c config.json --group-id com.capillary.coupongateway --model-package com.capillary.promotion.engine.models --api-package com.capillary.promotion.engine.api --artifact-id promotion-engine-swagger-sdk
  tar cvzf promotion_engine/java_client/java_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./promotion_engine/java_client/java/ .
  mvn3 clean deploy -f promotion_engine/java_client/java/pom.xml
  fpm -f -s "dir" -t "deb" -a "all" -n "java-swagger-promotion-engine-sdk" -v $BUILD_NUMBER -C ./promotion_engine/java_client --deb-no-default-config-files  java="/usr/share/java/capillary-libs/swagger_sdk_gen_promotion_engine"

elif [ "$Client" = "c#" ]
then rm -rf intouch_api/csharp_client/c#
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url \
  -l csharp\
  -DtargetFramework=v$Version \
  -o intouch_api/csharp_client/c#
  tar cvzf intouch_api/csharp_client/c#swagger_sdK_$BUILD_NUMBER.tar.gz -C ./intouch_api/csharp_client/c#/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "c#-swagger-promotion-engine-sdk" -v $BUILD_NUMBER -C ./intouch_api/csharp_client --deb-no-default-config-files  csharp="/usr/share/c#/capillary-libs/swagger-promotion-engine-sdk"
elif [ "$Client" = "php" ]
then rm -rf promotion_engine/php_client/php
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l php \
  -o promotion_engine/php_client/php \
  -c config_php.json
  tar cvzf promotion_engine/php_client/php_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/php_client/php/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "swagger-promotion-engine-sdk" -v $BUILD_NUMBER -C ./promotion_engine/php_client --deb-no-default-config-files  php="/usr/share/php/capillary-libs/swagger-promotion-engine-sdk"
elif [ "$Client" = "nodejs" ]
then rm -rf intouch_api/nodejs_client
	mkdir -p intouch_api/nodejs_client/
	 curl $url > swagger.json
     npm install swagger-js-codegen
     cd swagger-js-codegen
     node ../nodejs_sdk_gen > ../intouch_api/nodejs_client/node_$BUILD_NUMBER
     fpm -f -s "dir" -t "deb" -a "all" -n "node-swagger-promotion-engine-sdk" -v $BUILD_NUMBER -C ./intouch_api/nodejs_client --deb-no-default-config-files  nodejs="/usr/share/nodejs/capillary-libs/swagger-promotion-engine-sdk"
elif [ "$Client" = "python" ]
then rm -rf intouch_api/python_client
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l python \
  -o intouch_api/python_client/python
  tar cvzf intouch_api/python_client/python_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/python_client/python/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "py-swagger-promotion-engine-sdk" -v $BUILD_NUMBER -C ./intouch_api/python_client --deb-no-default-config-files  python="/usr/share/python/capillary-libs/swagger-promotion-engine-sdk"
else " no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"
