set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

echo "Typescript Petstore API client (default)"
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l typescript-fetch -o samples/client/petstore/typescript-fetch/default"
java %JAVA_OPTS% -jar %executable% %ags%

echo "Typescript Petstore API client (with interfaces generated)"
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l typescript-fetch -o samples/client/petstore/typescript-fetch/with-interfaces -D withInterfaces=true"
java %JAVA_OPTS% -jar %executable% %ags%

echo "Typescript Petstore API client (npm setting)"
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l typescript-fetch -c bin/typescript-fetch-petstore-with-npm.json -o samples/client/petstore/typescript-fetch/npm"
java %JAVA_OPTS% -jar %executable% %ags%