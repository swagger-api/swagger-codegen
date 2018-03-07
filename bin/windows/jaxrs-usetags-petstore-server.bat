set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate --artifact-id="swagger-jaxrs-jersey2-useTags" -t modules\swagger-codegen\src\main\resources\JavaJaxRS -i modules\swagger-codegen\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -l jaxrs -o samples\server\petstore\jaxrs\jersey2-useTags -DhideGenerationTimestamp=true,serverPort=8080 --additional-properties useTags=true

echo "Removing files and folders under samples/server/petstore/jaxrs/jersey2-useTags/src/main"
rd /S /Q samples\server\petstore\jaxrs\jersey2-useTags\src\main
for /f %%F in ('dir /b /a-d samples\server\petstore\jaxrs\jersey2-useTags ^| findstr /vleb "README.md"') do del /Q "samples\server\petstore\jaxrs\jersey2-useTags\%%F"
java %JAVA_OPTS% -jar %executable% %ags%
