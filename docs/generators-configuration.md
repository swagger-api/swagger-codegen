# Advanced Generator Customization

There are different aspects of customizing the code generator beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```sh
$ ls -1 modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/
AbstractJavaJAXRSServerCodegen.java
AbstractTypeScriptClientCodegen.java
... (results omitted)
TypeScriptAngularClientCodegen.java
TypeScriptNodeClientCodegen.java
```

Each of these files creates reasonable defaults so you can get running quickly.  But if you want to configure package names, prefixes, model folders, etc. you can use a json config file to pass the values.

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i https://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java \
  -c path/to/config.json
```

and `config.json` contains the following as an example:

```json
{
  "apiPackage" : "petstore"
}
```

Supported config options can be different per language. Running `config-help -l {lang}` will show available options.
**These options are applied via configuration file (e.g. config.json) or by passing them with `java -jar swagger-codegen-cli.jar -D{optionName}={optionValue}`**.

> If `-D{optionName}` does not work, please open a [ticket](https://github.com/swagger-api/swagger-codegen/issues/new) and we'll look into it.

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l java
```

Output

```text
CONFIG OPTIONS
modelPackage
    package for generated models

apiPackage
    package for generated api classes
...... (results omitted)
library
    library template (sub-template) to use:
    jersey1 - HTTP client: Jersey client 1.18. JSON processing: Jackson 2.4.2
    jersey2 - HTTP client: Jersey client 2.6
    feign - HTTP client: Netflix Feign 8.1.1.  JSON processing: Jackson 2.6.3
    okhttp-gson (default) - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1
    retrofit - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1 (Retrofit 1.9.0)
    retrofit2 - HTTP client: OkHttp 2.5.0. JSON processing: Gson 2.4 (Retrofit 2.0.0-beta2)
    google-api-client - HTTP client: google-api-client 1.23.0. JSON processing: Jackson 2.8.9
    rest-assured - HTTP client: rest-assured : 3.1.0. JSON processing: Gson 2.6.1. Only for Java8
```

Your config file for Java can look like

```json
{
  "groupId": "com.my.company",
  "artifactId": "MyClient",
  "artifactVersion": "1.2.0",
  "library": "feign"
}
```

For all the unspecified options default values will be used.

Another way to override default options is to extend the config class for the specific language.
To change, for example, the prefix for the Objective-C generated files, simply subclass the `ObjcClientCodegen.java`:

```java
package com.mycompany.swagger.codegen;

import io.swagger.codegen.languages.*;

public class MyObjcCodegen extends ObjcClientCodegen {
    static {
        PREFIX = "HELO";
    }
}
```

and specify the `classname` when running the generator:

```sh
-l com.mycompany.swagger.codegen.MyObjcCodegen
```

Your subclass will now be loaded and overrides the `PREFIX` value in the superclass.

## Bringing your own models

Sometimes you don't want a model generated.  In this case, you can simply specify an import mapping to tell
the codegen what _not_ to create.  When doing this, every location that references a specific model will
refer back to your classes.  Note, this may not apply to all languages...

To specify an import mapping, use the `--import-mappings` argument and specify the model-to-import logic as such:

```sh
--import-mappings Pet=my.models.MyPet
```

Or for multiple mappings:

```sh
--import-mappings Pet=my.models.MyPet,Order=my.models.MyOrder
```

or

```sh
--import-mappings Pet=my.models.MyPet --import-mappings Order=my.models.MyOrder
```
