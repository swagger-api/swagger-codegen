set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\fake-endpoints-v2/petstore.yaml -l ze-ph -o samples\server\petstore\ze-ph

java %JAVA_OPTS% -jar %executable% %ags%
