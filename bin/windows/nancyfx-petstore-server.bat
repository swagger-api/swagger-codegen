set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -Dlogback.configurationFile=bin/logback.xml
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l nancyfx -o samples\server\petstore\nancyfx\

REM java %JAVA_OPTS% -jar %executable% %ags%
