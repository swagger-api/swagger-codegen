package io.swagger.codegen.v3;

import io.swagger.v3.oas.models.media.Schema;

import java.util.List;

public interface ISchemaHandler {

    /**
     * check if there is any composed schema in models or properties. And create codegen model objects if it's required.
     * @param codegenModel
     * @param schema
     */
    void proccessComposedSchemas(CodegenModel codegenModel, Schema schema);

    /**
     * retrieve a list of codegen models created from composed schemas found.
     * @return
     */
    List<CodegenModel> getModels();
}
