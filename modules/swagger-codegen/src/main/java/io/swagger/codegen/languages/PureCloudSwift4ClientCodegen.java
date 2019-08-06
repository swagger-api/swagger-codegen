package io.swagger.codegen.languages;


import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Operation;
import io.swagger.models.properties.Property;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PureCloudSwift4ClientCodegen extends Swift4Codegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudSwift4ClientCodegen.class);

    public PureCloudSwift4ClientCodegen() {
        super();

        // Override output folder
        sourceFolder = "Classes" + File.separator + "PureCloud";

        // Use default templates
        embeddedTemplateDir = templateDir = "Swift";

        // Additional templates
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        // Custom mappings and overrides for swagger type -> swift type
        typeMapping.put("object", "JSON");
        typeMapping.put("LocalDateTime", "String");

        // Documentation

        // make api and model doc path available in mustache template
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
    }

    @Override
    public String getName() { return "purecloudios"; }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String type = p.baseType;
        if (type == null || type.equals(""))
            type = p.dataType;
        if (type != null && !type.equals("")) {
            if (p.defaultValue != null && !p.defaultValue.equals((""))) {
                p.example = p.defaultValue;
                if (type.toLowerCase().equals("string")) {
                    p.example = "\"" + p.example + "\"";
                }
            } else {
                switch (type.toLowerCase()) {
                    case "character":
                    case "string": {
                        p.example = "\"\"";
                        break;
                    }
                    case "int32":
                    case "int64":
                    case "int": {
                        p.example = "0";
                        break;
                    }
                    case "double":
                    case "float": {
                        p.example = "0";
                        break;
                    }
                    case "bool": {
                        p.example = "true";
                        break;
                    }
                    case "any": {
                        p.example = "[Any]";
                        break;
                    }
                    case "anyobject": {
                        p.example = "[AnyObject]";
                        break;
                    }
                    default: {
                        p.example = "new " + type + "(...)";
                    }
                }

                if (p.isListContainer != null && p.isListContainer) {
                    p.example = "[" + p.example + "]";
                }
            }
        }
    }

    @Override
    /**
     * Get the operation ID or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getVendorExtensions().containsKey(OPERATION_ID_PROPERTY_NAME)) {
            String operationId = operation.getVendorExtensions().get(OPERATION_ID_PROPERTY_NAME).toString();
            if (!StringUtils.isBlank(operationId)) {
                return operationId;
            }
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        String enumVarName = super.toEnumVarName(name, datatype);
        enumVarName = enumVarName.replaceAll("[\\W]", "_");
        return enumVarName.replace("*", "Wildcard");
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if (parameter.isEnum) {
            if (parameter.datatypeWithEnum.startsWith("["))
                parameter.enumName = parameter.datatypeWithEnum.substring(1, parameter.datatypeWithEnum.length() - 1);
            else
                parameter.enumName = parameter.datatypeWithEnum;

            if (parameter.allowableValues == null || !parameter.allowableValues.containsKey("values")) return;

            ArrayList values = (ArrayList)parameter.allowableValues.get("values");
            List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
            for (int i = 0; i < values.size(); i++) {
                Map<String, String> enumVar = new HashMap<String, String>();
                String s = values.get(i).toString();
                enumVar.put("value", s);

                // Replace non-alphanumeric chars in name with underscore
                s = toEnumVarName(s, "string");

                enumVar.put("name", s);
                enumVars.add(enumVar);
            }
            parameter.allowableValues.clear();
            parameter.allowableValues.put("enumVars", enumVars);
        }
    }

    @Override
    protected void postProcessProperty(CodegenProperty property) {
        super.postProcessProperty(property);

        if (property.isContainer != null && property.isContainer &&
            property.isListContainer != null && property.isListContainer &&
            property.containerType != null && property.containerType.equals("array") &&
            property.items != null && property.items.isEnum) {

            property.isEnum = true;

            if (property.datatypeWithEnum.startsWith("["))
                property.enumName = property.datatypeWithEnum.substring(1, property.datatypeWithEnum.length() - 1);
            else
                property.enumName = property.datatypeWithEnum;

            property.allowableValues = property.items.allowableValues;
        }
    }
}
