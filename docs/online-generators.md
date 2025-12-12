# Online generators

`swagger-generator` module exposes codegen as a web service, with it's own `swagger-js` based web UI, and available docker image `swaggerapi/swagger-generator-v3`.

The web service is deployed at https://generator3.swagger.io/ui, or it can be easily deployed as docker container.

The OpenAPI specification of generator service APIs are available either via UI exposed by web service (e.g. https://generator3.swagger.io/ui), as exposed YAML (https://generator3.swagger.io/openapi.json)
or in source code repo (https://github.com/swagger-api/swagger-codegen/blob/3.0.0/modules/swagger-generator/src/main/resources/openapi.yaml).

> Please note that both V2 (for v2 specs) and V3 generators (for v3 and v2 specs converted during generation) are supported, by providing property `codegenVersion` (e.g `"codegenVersion" : "v3"`).

For example, to generate a java API client, simply send the following HTTP request using curl:

```sh
curl -X POST \
  https://generator3.swagger.io/api/generate \
  -H 'content-type: application/json' \
  -d '{
  "specURL" : "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v3.0/petstore.yaml",
  "lang" : "java",
  "type" : "CLIENT",
  "codegenVersion" : "V3"
}'
```

The response will contain a zipped file containing the generated code.

To customize the SDK, you can specify language specific options  with the following HTTP body:

```json
{
  "specURL" : "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v3.0/petstore.yaml",
  "lang" : "java",
  "options" : {
    "additionalProperties" : {
    "useRuntimeException": true,
    "useRxJava" : true
    }
  },
  "type" : "CLIENT",
  "codegenVersion" : "V3"
}
```

in which the `options` `additionalProperties` for a language can be obtained by submitting a `GET` request to `https://generator3.swagger.io/api/options?language={language}&version={codegenVersion}`:

For example, `curl https://generator3.swagger.io/api/options?language=java&version=V3` returns (truncated output):

```json
{
  "sortParamsByRequiredFlag": {
    "opt": "sortParamsByRequiredFlag",
    "description": "Sort method arguments to place required parameters before optional parameters.",
    "type": "boolean",
    "default": "true"
  },
  "ensureUniqueParams": {
    "opt": "ensureUniqueParams",
    "description": "Whether to ensure parameter names are unique in an operation (rename parameters that are not).",
    "type": "boolean",
    "default": "true"
  },
  "allowUnicodeIdentifiers": {
    "opt": "allowUnicodeIdentifiers",
    "description": "boolean, toggles whether unicode identifiers are allowed in names or not, default is false",
    "type": "boolean",
    "default": "false"
  },
  "modelPackage": {
    "opt": "modelPackage",
    "description": "package for generated models",
    "type": "string"
  },
  ...
```

Instead of using `specURL` with an URL to the OpenAPI/Swagger spec, one can include the spec in the JSON payload with `spec`, e.g.

```json
{
  "options": {},
  "spec": {
    "swagger": "2.0",
    "info": {
      "version": "1.0.0",
      "title": "Test API"
    },
    ...
  }
}
```
