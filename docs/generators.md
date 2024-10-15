# Generators

If the default generator configuration does not meet your needs, you have various options to modify or create new modules or templates.

## Modifying the client library format

Don't like the default swagger client syntax?  Want a different language supported?  No problem!  

Swagger Codegen processes handlebar templates with the [Handlebars.java](https://github.com/jknack/handlebars.java) engine.  You can modify our templates or make your own.

Take a look at [swagger-codegen-generators](https://github.com/swagger-api/swagger-codegen-generators/tree/master/src/main/resources/handlebars) for examples. To make your own templates, create your own files and use the `-t` flag to specify your template folder.  It actually is that easy!

## Making your own codegen modules

If you're starting a project with a new language and don't see what you need, Swagger Codegen can help you create a project to generate your own libraries:

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar meta \
  -o output/myLibrary -n myClientCodegen -p com.my.company.codegen
```

This will write, in the folder `output/myLibrary`, all the files you need to get started, including a `README.md`. Once modified and compiled, you can load your library with the codegen and generate clients with your own, custom-rolled logic.

You would then compile your library in the `output/myLibrary` folder with `mvn package` and execute the codegen like such:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.v3.cli.SwaggerCodegen
```

For Windows users, you will need to use `;` instead of `:` in the classpath, e.g.:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar;modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.v3.cli.SwaggerCodegen
```

Note the `myClientCodegen` is an option now, and you can use the usual arguments for generating your library:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar \
  io.swagger.codegen.v3.cli.SwaggerCodegen generate -l myClientCodegen\
  -i http://petstore.swagger.io/v2/swagger.json \
  -o myClient
```

See also [standalone generator development](https://github.com/swagger-api/swagger-codegen/blob/3.0.0/standalone-gen-dev/standalone-generator-development.md).

## Generating a client from local files

If you don't want to call your server, you can save the OpenAPI Description files into a directory and pass an argument
to the code generator like this:

```sh
-i ./modules/swagger-codegen/src/test/resources/2_0/petstore.json
```

Great for creating libraries on your CI server, from the [Swagger Editor](http://editor.swagger.io) ... or while coding on an airplane ✈️.
