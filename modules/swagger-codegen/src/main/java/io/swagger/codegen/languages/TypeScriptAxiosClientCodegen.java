package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.ModelImpl;

import java.io.File;
import java.util.*;

public class TypeScriptAxiosClientCodegen extends AbstractTypeScriptClientCodegen {
    public TypeScriptAxiosClientCodegen() {
        super();

        this.outputFolder = "generated-code/typescript-axios";
        embeddedTemplateDir = templateDir = "typescript-axios";

        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");

        modelPackage = "models";
        apiPackage = "resources";
    }

    @Override
    public String getName() {
        return "typescript-axios";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using Axios for HTTP requests.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("axios.config.mustache", "axios.config.ts"));
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toModelImport(String name) {
        return modelPackage() + "/" + toModelFilename(name);
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(getSymbolName(name));
        }

        // number
        if ("number".equals(datatype)) {
            String varName = "NUMBER_" + name;

            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        String enumName = name;
        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessModels(objs);

        // Add additional filename information for imports
        List<Map<String, Object>> models = (List<Map<String, Object>>)postProcessModelsEnum(result).get("models");
        for (Map<String,Object> mo : models) {
            CodegenModel cm = (CodegenModel) mo.get("model");
            mo.put("tsImports", toTsImports(cm.imports));
        }

        return result;
    }

    private List<Map<String, String>> toTsImports(Set<String> imports) {
        List<Map<String, String>> tsImports = new ArrayList<>();
        for(String im : imports) {
            HashMap<String, String> tsImport = new HashMap<>();
            tsImport.put("classname", im);
            tsImport.put("filename", toModelFilename(im));
            tsImports.add(tsImport);
        }
        return tsImports;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        // Add additional filename information for model imports in the services
        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for(Map<String, Object> im : imports) {
            im.put("filename", im.get("import"));
            im.put("classname", getModelnameFromModelFilename(im.get("filename").toString()));
        }

        // if there are any form params, we'll need to import stringify
        Map<String, Object> opsObj = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>)opsObj.get("operation");
        boolean anyHasFormParams = false;
        for (CodegenOperation op : ops) {
            if (op.getHasFormParams()) {
                anyHasFormParams = true;
            }
        }
        if (anyHasFormParams) {
            operations.put("hasFormParams", true);
        }

        return operations;
    }

    private String getModelnameFromModelFilename(String filename) {
        return filename.substring((modelPackage() + "/").length());
    }
}
