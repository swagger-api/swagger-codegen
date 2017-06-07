# Typescript Angular2 Swagger CodeGen Template

Angular 2 typescript template for [swagger-codegen](https://github.com/swagger-api/swagger-codegen)

### Usage

After cloning the repository run

```bash
swagger-codegen generate \
    -i http://petstore.swagger.io/v2/swagger.json \
    -o client/petstore \
    -l typescript-angular \
    -t ./TypeScript-Angular2
```