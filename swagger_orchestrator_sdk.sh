if $Build_Codegen; then
  mvn3 clean package -Dmaven.test.skip=true
fi

echo "{\"artifactVersion\":\"1.42-SNAPSHOT\"}" >config.json
echo "GENERATING SDK"

if [ "$Branch" = "snapshot" ]; then
  url="https://orchestratoradmin.crm-nightly-new.cctools.capillarytech.com/v3/api-docs"
  version="https://orchestratoradmin.crm-nightly-new.cctools.capillarytech.com/v3/meta/version"
elif [ "$Branch" = "production" ]; then
  url="https://orchestratoradmin.crm-staging-new.cctools.capillarytech.com/v2/api-docs"
  version="https://orchestratoradmin.crm-staging-new.cctools.capillarytech.com/v3/meta/version"
else
  "No Branch is selected"
fi

if [ "$Client" = "java" ]; then
  rm -rf orchestrator/java_client/java
  mkdir -p orchestrator/java_client/java
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
    -i orchestrator.json \
    -l java \
    -DdateLibrary=java11 \
    -o orchestrator/java_client/java \
    -c config.json --group-id com.capillary.promotion.engine --model-package com.capillary.promotion.engine.models --api-package com.capillary.promotion.engine.api --artifact-id promotion-engine-swagger-sdk
  tar cvzf orchestrator/java_client/java_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./orchestrator/java_client/java/ .
  mvn3 clean deploy -f orchestrator/java_client/java/pom.xml
  fpm -f -s "dir" -t "deb" -a "all" -n "java-swagger-orchestrator-sdk" -v $BUILD_NUMBER -C ./orchestrator/java_client --deb-no-default-config-files java="/usr/share/java/capillary-libs/swagger-orchestrator-sdk"

elif [ "$Client" = "php" ]; then
  rm -rf orchestrator/php_client/php
  mkdir -p orchestrator/php_client/php
  java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
    -i orchestrator.json \
    -l php \
    -o orchestrator/php_client/php \
    -c config.json
  tar cvzf orchestrator/php_client/php_swagger_sdk_$BUILD_NUMBER.tar.gz -C ./orchestrator/php_client/php/ .
  fpm -f -s "dir" -t "deb" -a "all" -n "swagger-orchestrator-sdk" -v $BUILD_NUMBER -C ./orchestrator/php_client --deb-no-default-config-files php="/usr/share/php/capillary-libs/swagger-orchestrator-sdk"
else
  "no client is selected"
fi
echo "SWAGGER SDK SUCCESSFULLY GENERATED"
