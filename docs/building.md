# Building

After cloning the project, you can build it from source with this command:

```sh
mvn clean package
```

## Homebrew

To install, run `brew install swagger-codegen`

Here is an example usage:

```sh
swagger-codegen generate -i http://petstore.swagger.io/v2/swagger.json -l ruby -o /tmp/test/
```

## To build a server stub

Please refer to [wiki](https://github.com/swagger-api/swagger-codegen/wiki/Server-stub-generator-HOWTO) for more information.

## To build the codegen library

This will create the Swagger Codegen library from source.

```sh
mvn package
```

> The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts.
