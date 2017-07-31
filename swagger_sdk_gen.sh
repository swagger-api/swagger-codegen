if $Build_Codegen ; then
    mvn3 clean package -Dmaven.test.skip=true
fi
if [ "$Environment" = "Nightly" ]
  then
    $url = "http://newapi.nightly.capillary.in/swagger.json"
    $version ="http://newapi.nightly.capillary.in/version.json"
elif ["$Environment" = "Staging"]
  then
    $url = "http://newapi.staging.capillary.in/swagger.json"
    $version ="http://newapi.staging.capillary.in/version.json"
elif ["$Environment" = "APAC2"]
  then
elif ["$Environment" = "INDIA"]
  then
  $url ="http://newapi.capillary.co.in/swagger.json"
  $version ="http://newapi.capillary.co.in/version.json"
elif ["$Environment" = "CHINA"]
  then 
      $url = "http://newapi.capillarytech.cn.com/swagger.json"
      $version ="http://http://newapi.capillarytech.cn.com/version.json"
elif ["$Environment" = "EU"]
  then
  $url = "http://newapi.capillarytech.cn.com/swagger.json"
  $version ="http://http://newapi.eu.capillarytech.com/version.json"
else " No Environment is selected"
fi
  #statements
curl -k $version -o config.json
echo "GENERATING SDK"
if [ "$Client" = "java" ]
then 
  rm -rf intouch_api/java_client/java
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://newapi.nightly.capillary.in/swagger.json  \
  -l java \
  -DdateLibrary=java8 \
  -o intouch_api/java_client/java \
  -c config.json
  tar cvzf intouch_api/java_client/java_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/java_client/java/ .
  mvn3 clean deploy -f intouch_api/java_client/java/pom.xml
elif [ "$Client" = "c#" ]
then rm -rf intouch_api/csharp_client/c#
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://newapi.nightly.capillary.in/swagger.json \
  -l csharp\
  -DtargetFramework=v$Version \
  -o intouch_api/csharp_client/c#
  tar cvzf intouch_api/csharp_client/c#swagger_sdK_$BUILD_NUMBER.tar.gz -C ./intouch_api/csharp_client/c#/ .
elif [ "$Client" = "php" ]
then rm -rf intouch_api/php_client/php
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l php \
  -o intouch_api/php_client/php
  tar cvzf intouch_api/php_client/php_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/php_client/php/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "swagger-sdk" -v $BUILD_NUMBER -C ./intouch_api/php_client --deb-no-default-config-files  php="/usr/share/php/capillary-libs/swagger-sdk"
elif [ "$Client" = "nodejs" ]
then rm -rf intouch_api/nodejs_client
	mkdir -p intouch_api/nodejs_client/
	 curl http://newapi.nightly.capillary.in/swagger.json > swagger.json
     npm install swagger-js-codegen
     cd swagger-js-codegen
     node ../nodejs_sdk_gen > ../intouch_api/nodejs_client/node_$BUILD_NUMBER
else " no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"
