# Building

After cloning the project, you can build it from source with this command:

```sh
mvn clean package
```

If you don't have maven installed, you may directly use the included [maven wrapper](https://github.com/takari/maven-wrapper), and build with the command:

```sh
./mvnw clean package
```

## Homebrew

To install, run `brew install swagger-codegen`

Here is an example usage:

```sh
swagger-codegen generate -i https://petstore.swagger.io/v2/swagger.json -l ruby -o /tmp/test/
```

## To build a server stub

Please refer to https://github.com/swagger-api/swagger-codegen/wiki/Server-stub-generator-HOWTO for more information.

## To build the codegen library

This will create the Swagger Codegen library from source.

```sh
mvn package
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts.
