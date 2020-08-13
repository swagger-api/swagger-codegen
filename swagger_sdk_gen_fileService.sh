if $Build_Codegen ; then
    mvn3 clean package -Dmaven.test.skip=true
fi
if [ "$Branch" = "master" ]
  then
    url="https://crm-nightly-new.cc.capillarytech.com/api_gateway/file-service/v2/api-docs"
    version="https://crm-nightly-new.cc.capillarytech.com/api_gateway/file-service/v1/meta/version"
elif [ "$Branch" = "production" ]
  then
    url="http://fileservice.capillary.in/tl-docs-test/v2/api-docs"
    version="https://fileservice.capillary.in/v1/meta/version"
else " No Branch is selected"
fi
ver=$(curl -H"Authorization: Basic a3Jpc2huYS50aWxsMDE6MjAyY2I5NjJhYzU5MDc1Yjk2NGIwNzE1MmQyMzRiNzA=" -k $version)
echo {"artifactVersion":$ver} > config.json
echo "GENERATING SDK"
if [ "$Client" = "java" ]
then 
  rm -rf fileservice/java_client/java
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -a "Authorization%3ABasic%20a3Jpc2huYS50aWxsMDE6MjAyY2I5NjJhYzU5MDc1Yjk2NGIwNzE1MmQyMzRiNzA%3D"
  -i $url  \
  -l java \
  -DdateLibrary=java8 \
  -o fileservice/java_client/java \
  -c config.json
  tar cvzf fileservice/java_client/java_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./filservice/java_client/java/ .
  mvn3 clean deploy -f fileservice/java_client/java/pom.xml
  fpm -f -s "dir" -t "deb" -a "all" -n "swagger_sdk_gen_fileService" -v $BUILD_NUMBER -C ./fileservice/java_client --deb-no-default-config-files  java="/usr/share/java/capillary-libs/swagger_sdk_gen_fileService"
else " no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"
