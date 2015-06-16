java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate -i http://192.168.0.16:4529/swagger/docs/v1 -l android -o easyDocs/client/android
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate -i http://192.168.0.16:4529/swagger/docs/v1 -l csharp -o easyDocs/client/csharp
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate -i http://192.168.0.16:4529/swagger/docs/v1 -l objc -o easyDocs/client/ios
