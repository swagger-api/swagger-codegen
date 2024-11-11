# Generators

If the default generator configuration does not meet your needs, you have various options to modify or create new modules or templates.

## Modifying the client library format

Don't like the default swagger client syntax?  Want a different language supported?  No problem!  Swagger Codegen processes mustache templates with the [jmustache](https://github.com/samskivert/jmustache) engine.  You can modify our templates or make your own.

You can look at `modules/swagger-codegen/src/main/resources/${your-language}` for examples.  To make your own templates, create your own files and use the `-t` flag to specify your template folder.  It actually is that easy.

## Making your own codegen modules

If you're starting a project with a new language and don't see what you need, Swagger Codegen can help you create a project to generate your own libraries:

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar meta \
  -o output/myLibrary -n myClientCodegen -p com.my.company.codegen
```

This will write, in the folder `output/myLibrary`, all the files you need to get started, including a `README.md`. Once modified and compiled, you can load your library with the codegen and generate clients with your own, custom-rolled logic.

You would then compile your library in the `output/myLibrary` folder with `mvn package` and execute the codegen like such:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.SwaggerCodegen
```

For Windows users, you will need to use `;` instead of `:` in the classpath, e.g.:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar;modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.SwaggerCodegen
```

Note the `myClientCodegen` is an option now, and you can use the usual arguments for generating your library:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar \
  io.swagger.codegen.SwaggerCodegen generate -l myClientCodegen\
  -i https://petstore.swagger.io/v2/swagger.json \
  -o myClient
```

See also [standalone generator development](https://github.com/swagger-api/swagger-codegen/blob/master/standalone-gen-dev/standalone-generator-development.md).

## Generating a client from local files

If you don't want to call your server, you can save the OpenAPI Spec files into a directory and pass an argument
to the code generator like this:

```sh
-i ./modules/swagger-codegen/src/test/resources/2_0/petstore.json
```

Great for creating libraries on your ci server, from the [Swagger Editor](http://editor.swagger.io)... or while coding on an airplane ✈️.

## Ignore file format

Swagger Codegen supports a `.swagger-codegen-ignore` file, similar to `.gitignore` or `.dockerignore` you're probably already familiar with.

The ignore file allows for better control over overwriting existing files than the `--skip-overwrite` flag. With the ignore file, you can specify individual files or directories can be ignored. This can be useful, for example if you only want a subset of the generated code.

Examples:

```sh
# Swagger Codegen Ignore
# Lines beginning with a # are comments

# This should match build.sh located anywhere.
build.sh

# Matches build.sh in the root
/build.sh

# Exclude all recursively
docs/**

# Explicitly allow files excluded by other rules
!docs/UserApi.md

# Recursively exclude directories named Api
# You can't negate files below this directory.
src/**/Api/

# When this file is nested under /Api (excluded above),
# this rule is ignored because parent directory is excluded by previous rule.
!src/**/PetApiTests.cs

# Exclude a single, nested file explicitly
src/IO.Swagger.Test/Model/AnimalFarmTests.cs
```

The `.swagger-codegen-ignore` file must exist in the root of the output directory.

Upon first code generation, you may also pass the CLI option `--ignore-file-override=/path/to/ignore_file` for greater control over generated outputs. Note that this is a complete override, and will override the `.swagger-codegen-ignore` file in an output directory when regenerating code.

Editor support for `.swagger-codegen-ignore` files is available in IntelliJ via the [.ignore plugin](https://plugins.jetbrains.com/plugin/7495--ignore).
