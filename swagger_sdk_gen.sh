if $Build_Codegen ; then
    mvn3 clean package -Dmaven.test.skip=true
fi
echo "GENERATING SDK"
if [ "$Client" = "c#" ]
then java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://newapi.nightly.capillary.in/swagger.json \
  -l csharp\
  -DtargetFramework=v$Version \
  -o samples/client/intouch_api/csharp_client/c#_$BUILD_NUMBER
  tar cvzf samples/client/intouch_api/csharp_client/c#swagger_sdK_$BUILD_NUMBER.tar.gz -C ./samples/client/intouch_api/csharp_client/c#_$BUILD_NUMBER/ .
elif [ "$Client" = "php" ]
then java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://newapi.nightly.capillary.in/swagger.json  \
  -l php \
  -o samples/client/intouch_api/php_client/php_$BUILD_NUMBER
  tar cvzf samples/client/intouch_api/php_client/php_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./samples/client/intouch_api/php_client/php_$BUILD_NUMBER/ .
elif [ "$Client" = "nodejs" ]
then mkdir -p samples/client/intouch_api/nodejs_client
	 curl http://newapi.nightly.capillary.in/swagger.json > swagger.json
     npm install swagger-js-codegen
     cd swagger-js-codegen
     node ../nodejs_sdk_gen > ../samples/client/intouch_api/nodejs_client/node_client/node_$BUILD_NUMBER
     tar cvzf /samples/client/intouch_api/nodejs_client/node_client/node_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./samples/client/intouch_api/nodejs_client/node_client/node_$BUILD_NUMBER/ .
else " no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"