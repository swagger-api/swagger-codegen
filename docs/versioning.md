# Versioning

Both 2.X and 3.X version lines of Swagger Codegen are available and are independently maintained.

**NOTE:** version 2.X (`io.swagger`) and 3.X (`io.swagger.codegen.v3`) have **different** group ids.

## Swagger Codegen 2.X

|||
|----|-----|
|Version:| 2.X|
|Repository branch:|[master](https://github.com/swagger-api/swagger-codegen/tree/master)|
|groupId:| `io.swagger`|
|Swagger/OpenAPI support:| 2.0|
|maven central (maven plugin):|[swagger-codegen-maven-plugin](https://mvnrepository.com/artifact/io.swagger/swagger-codegen-maven-plugin)|

dependency example:

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
    <version>2.4.44</version>
</dependency>
```

## Swagger Codegen 3.X

|||
|----|-----|
|Version:| 3.X|
|Repository branch:|[3.0.0](https://github.com/swagger-api/swagger-codegen/tree/3.0.0)|
|groupId:| `io.swagger.codegen.v3`|
|Swagger/OpenAPI support:| 2.0 (_by using engine + generators of 2.X_), 3.0.X|
|maven central:|[io.swagger.codegen.v3](https://mvnrepository.com/artifact/io.swagger.codegen.v3)

dependency example:

```xml
<dependency>
    <groupId>io.swagger.codegen.v3</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
    <version>3.0.61</version>
</dependency>
```
