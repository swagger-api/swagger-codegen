set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM Test spec files don't get overwritten, so delete them first.
del /S samples\client\petstore\javascript-es6\test\*.spec.js

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -l javascript -o samples\client\petstore\javascript-es6 --additional-properties useES6=true

java -DappName=PetstoreClient %JAVA_OPTS% -jar %executable% %ags%
