if $Build_Codegen ; then
    mvn3 clean package -Dmaven.test.skip=true
fi
if [ "$Branch" = "snapshot" ]
  then
    url="https://intouch-api-v3-swagger.crm-nightly-new.cc.capillarytech.com/v3/api-docs/v3"
    version="https://intouch-api-v3-swagger.crm-nightly-new.cc.capillarytech.com/v3/meta/version"
elif [ "$Branch" = "production" ]
  then
    url="https://intouch-api-v3-swagger.crm-staging-new.cc.capillarytech.com/v3/api-docs"
    version="https://intouch-api-v3-swagger.crm-staging-new.cc.capillarytech.com/v3/meta/version"
else " No Branch is selected"
fi
curl -k $version -o config.json
#echo '{"artifactVersion":"0.0.1-SNAPSHOT","invokerPackage":"SwaggerV3\\\\Client","modelPackage":"SwaggerV3\\\\Client\\\\Model","apiPackage":"SwaggerV3\\\\Client\\\\Api"}'>config_php.json
echo "GENERATING SDK"
if [ "$Client" = "java" ]
then
    echo "Removing existing Java client SDK..."
    rm -rf intouch_api/java_client/java

    # Step 2: Generate the SDK using OpenAPI Generator (via Docker)
    echo "Generating SDK using OpenAPI Generator..."
    docker run --rm -v $(pwd):/local openapitools/openapi-generator-cli generate \
        -i /local/$url \
        -g java \
        -p dateLibrary=java8 \
        -o /local/intouch_api/java_client/java \
        -c /local/config.json

    # Step 3: Create a .tar.gz archive of the generated SDK
    echo "Creating .tar.gz archive..."
    tar cvzf intouch_api/java_client/java_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/java_client/java/ .

    # Step 4: Deploy the SDK using Maven
    echo "Deploying SDK using Maven..."
    mvn3 clean deploy -f intouch_api/java_client/java/pom.xml

    # Step 5: Create a .deb package using fpm
    echo "Creating .deb package using fpm..."
    fpm -f -s "dir" -t "deb" -a "all" -n "java-swagger-v3-sdk" -v $BUILD_NUMBER \
        -C ./intouch_api/java_client \
        --deb-no-default-config-files \
        java="/usr/share/java/capillary-libs/swagger-v3-sdk"

    echo "Build and deployment completed successfully."

elif [ "$Client" = "c#" ]
then rm -rf intouch_api/csharp_client/c#
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url \
  -l csharp\
  -DtargetFramework=v$Version \
  -o intouch_api/csharp_client/c#
  tar cvzf intouch_api/csharp_client/c#swagger_sdK_$BUILD_NUMBER.tar.gz -C ./intouch_api/csharp_client/c#/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "c#-swagger-v3-sdk" -v $BUILD_NUMBER -C ./intouch_api/csharp_client --deb-no-default-config-files  csharp="/usr/share/c#/capillary-libs/swagger-v3-sdk"
elif [ "$Client" = "php" ]
then rm -rf intouch_api/php_client/php
   java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l php \
  -o intouch_api/php_client/php \
  -c config_php.json
  tar cvzf intouch_api/php_client/php_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/php_client/php/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "swagger-v3-sdk" -v $BUILD_NUMBER -C ./intouch_api/php_client --deb-no-default-config-files  php="/usr/share/php/capillary-libs/swagger-v3-sdk"
elif [ "$Client" = "nodejs" ]
then rm -rf intouch_api/nodejs_client
	mkdir -p intouch_api/nodejs_client/
	 curl $url > swagger.json
     npm install swagger-js-codegen
     cd swagger-js-codegen
     node ../nodejs_sdk_gen > ../intouch_api/nodejs_client/node_$BUILD_NUMBER
     fpm -f -s "dir" -t "deb" -a "all" -n "node-swagger-v3-sdk" -v $BUILD_NUMBER -C ./intouch_api/nodejs_client --deb-no-default-config-files  nodejs="/usr/share/nodejs/capillary-libs/swagger-v3-sdk"
elif [ "$Client" = "python" ]
then rm -rf intouch_api/python_client
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i $url  \
  -l python \
  -o intouch_api/python_client/python
  tar cvzf intouch_api/python_client/python_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./intouch_api/python_client/python/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "py-swagger-v3-sdk" -v $BUILD_NUMBER -C ./intouch_api/python_client --deb-no-default-config-files  python="/usr/share/python/capillary-libs/swagger-v3-sdk"
else " no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"
