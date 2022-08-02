- put jar artifact of swagger-codegen-generator repo into the folder /osome-repository
- install it into local maven repo with:

```
    mvn deploy:deploy-file -Dfile=osome-repository/swagger-codegen-generators-1.0.36-SNAPSHOT.jar -DgroupId=io.swagger.codegen.v3 -DartifactId=swagger-codegen-generators -Dversion=1.0.36-SNAPSHOT -Dpackaging=jar -Durl=file:./osome-repository/ -DrepositoryId=osome-repository -DupdateReleaseInfo=true
```

- build codegen-cli with:

```
    mvn -am -pl "modules/swagger-codegen-cli" package
```

- .jar executable artifact will be created at the path `modules/swagger-codegen-cli/target/swagger-codegen-cli.jar`
