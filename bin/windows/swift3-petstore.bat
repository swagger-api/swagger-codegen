set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\fake-endpoints-v2/petstore.yaml -l swift3 -o samples\client\petstore\swift3\default

java %JAVA_OPTS% -jar %executable% %ags%
