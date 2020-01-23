package io.swagger.codegen.v3;

import io.swagger.v3.oas.models.media.Schema;

public interface ISchemaHandler {

    /**
     * check if there is any composed schema in models or properties. And create codegen model objects if it's required.
     * @param codegenModel
     * @param schema
     */
    void proccessComposedSchemas(CodegenModel codegenModel, Schema schema);
}
