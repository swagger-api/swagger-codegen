
# Online generators

One can also generate API client or server using the online generators (https://generator.swagger.io)

For example, to generate Ruby API client, simply send the following HTTP request using curl:

```sh
curl -X POST -H "content-type:application/json" -d '{"swaggerUrl":"https://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/ruby
```

Then you will receive a JSON response with the URL to download the zipped code.

To customize the SDK, you can `POST` to `https://generator.swagger.io/api/gen/clients/{language}` with the following HTTP body:

```json
{
  "options":  {},
  "swaggerUrl": "https://petstore.swagger.io/v2/swagger.json"
}
```

in which the `options` for a language can be obtained by submitting a `GET` request to `https://generator.swagger.io/api/gen/clients/{language}`:

For example, `curl https://generator.swagger.io/api/gen/clients/python` returns

```json
{
  "packageName": {
    "opt": "packageName",
    "description": "python package name (convention: snake_case).",
    "type": "string",
    "default": "swagger_client"
  },
  "packageVersion": {
    "opt": "packageVersion",
    "description": "python package version.",
    "type": "string",
    "default": "1.0.0"
  },
  "sortParamsByRequiredFlag": {
    "opt": "sortParamsByRequiredFlag",
    "description": "Sort method arguments to place required parameters before optional parameters.",
    "type": "boolean",
    "default": "true"
  }
}
```

To set package name to `pet_store`, the HTTP body of the request is as follows:

```json
{
  "options": {
    "packageName": "pet_store"
  },
  "swaggerUrl": "https://petstore.swagger.io/v2/swagger.json"
}
```

and here is the curl command:

```sh
curl -H "Content-type: application/json" -X POST -d '{"options": {"packageName": "pet_store"},"swaggerUrl": "https://petstore.swagger.io/v2/swagger.json"}' https://generator.swagger.io/api/gen/clients/python
```

Instead of using `swaggerUrl` with an URL to the OpenAPI/Swagger spec, one can include the spec in the JSON payload with `spec`, e.g.

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
