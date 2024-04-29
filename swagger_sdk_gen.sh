if $Build_Codegen ; then
    mvn3 clean package -Dmaven.test.skip=true
fi
if [ "$Branch" = "snapshot" ]
  then
    url="https://intouch-api-swagger.crm-nightly-new.cc.capillarytech.com/swagger.json"
    version="https://newapi.crm-nightly-new.cc.capillarytech.com/version"
elif [ "$Branch" = "production" ]
  then
    url="https://intouch-api-swagger.crm-staging-new.cc.capillarytech.com/swagger.json"
    version="https://newapi.intouch-a-crm-crm-staging-new.cc.capillarytech.com/version"
else " No Branch is selected"
fi
curl -k $version -o config.json
echo "GENERATING SDK"
if [ "$Client" = "java" ]
then 
  rm -rf intouch_api/java_client/java
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l java \
  -DdateLibrary=java8 \
  -o intouch_api/java_client/java \
  -c config.json
  tar cvzf intouch_api/java_client/java_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/java_client/java/ .
  mvn3 clean deploy -f intouch_api/java_client/java/pom.xml
  fpm -f -s "dir" -t "deb" -a "all" -n "java-swagger-sdk" -v $BUILD_NUMBER -C ./intouch_api/java_client --deb-no-default-config-files  java="/usr/share/java/capillary-libs/swagger-sdk"
elif [ "$Client" = "c#" ]
then rm -rf intouch_api/csharp_client/c#
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url \
  -l csharp\
  -DtargetFramework=v$Version \
  -o intouch_api/csharp_client/c#
  tar cvzf intouch_api/csharp_client/c#swagger_sdK_$BUILD_NUMBER.tar.gz -C ./intouch_api/csharp_client/c#/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "csharp-swagger-sdk" -v $BUILD_NUMBER -C ./intouch_api/csharp_client --deb-no-default-config-files  c#="/usr/share/c#/capillary-libs/swagger-sdk"
elif [ "$Client" = "php" ]
then rm -rf intouch_api/php_client/php
  if [ "$CamelCase" = "Yes" ]
    then
      echo "setting camelcase variable name convention for php sdk"
      echo '{"variableNamingConvention":"camelCase"}' > config.json
  fi
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l php \
  -c config.json \
  -o intouch_api/php_client/php
  tar cvzf intouch_api/php_client/php_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/php_client/php/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "swagger-sdk" -v $BUILD_NUMBER -C ./intouch_api/php_client --deb-no-default-config-files  php="/usr/share/php/capillary-libs/swagger-sdk"
elif [ "$Client" = "nodejs" ]
then rm -rf intouch_api/nodejs_client
	mkdir -p intouch_api/nodejs_client/
	 curl $url > swagger.json
     npm install swagger-js-codegen
     cd swagger-js-codegen
     node ../nodejs_sdk_gen > ../intouch_api/nodejs_client/node_$BUILD_NUMBER
     fpm -f -s "dir" -t "deb" -a "all" -n "node-swagger-sdk" -v $BUILD_NUMBER -C ./intouch_api/nodejs_client --deb-no-default-config-files  nodejs="/usr/share/nodejs/capillary-libs/swagger-sdk"
elif [ "$Client" = "python" ]
then rm -rf intouch_api/python_client
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l python \
  -o intouch_api/python_client/python
  tar cvzf intouch_api/python_client/python_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/python_client/python/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "py-swagger-sdk" -v $BUILD_NUMBER -C ./intouch_api/python_client --deb-no-default-config-files  python="/usr/share/python/capillary-libs/swagger-sdk"
else " no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"
