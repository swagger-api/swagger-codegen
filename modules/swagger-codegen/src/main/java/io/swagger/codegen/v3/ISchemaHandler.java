package io.swagger.codegen.v3;

import io.swagger.v3.oas.models.media.Schema;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface ISchemaHandler {

    default void readProcessedModels(Map<String,Object> allProcessedModels) {
        final List<CodegenSchema> codegenSchemas = new ArrayList<>();
        final Map<String, CodegenModel> allModels = new HashMap<>();
        for (String name : allProcessedModels.keySet()) {
            final Map<String, Object> modelMap = (Map<String, Object>) allProcessedModels.get(name);
            final List<Map<String, Object>> models = (List<Map<String, Object>>) modelMap.get("models");
            for (Map<String, Object> mo : models) {
                final CodegenModel codegenModel = (CodegenModel) mo.get("model");
                final Schema schema = (Schema) mo.get("schema");

                codegenSchemas.add(new CodegenSchema(codegenModel, schema));
                allModels.put(codegenModel.classname, codegenModel);
            }
        }
        if (codegenSchemas.isEmpty()) {
            return;
        }
        for (CodegenSchema codegenSchema : codegenSchemas) {
            processComposedSchemas(codegenSchema.getCodegenModel(), codegenSchema.getSchema(), allModels);
        }
    }

    /**
     * check if there is any composed schema in models or properties. And create codegen model objects if it's required.
     * @param codegenModel
     * @param schema
     */
    void processComposedSchemas(CodegenModel codegenModel, Schema schema, Map<String, CodegenModel> allModels);

    /**
     * retrieve a list of codegen models created from composed schemas found.
     * @return
     */
    List<CodegenModel> getModels();
}
