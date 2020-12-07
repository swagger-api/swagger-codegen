package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class PureCloudKotlinClientCodegen extends KotlinClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudKotlinClientCodegen.class);

    public PureCloudKotlinClientCodegen() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "Kotlin";

        // Custom mappings for swagger type -> kotlin type
        importMapping.put("LocalDateTime", "org.joda.time.LocalDateTime");
        importMapping.put("PagedResource", "com.mypurecloud.sdk.v2.PagedResource");
        importMapping.put("ArrayNode", "com.fasterxml.jackson.databind.node.ArrayNode");
        importMapping.put("LocalDate", "kotlin.time.LocalDate");
        importMapping.put("JsonNode", "com.fasterxml.jackson.databind.JsonNode");

        // Type overrides
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("float", "Float");
        typeMapping.put("long", "Long");
        typeMapping.put("double", "Double");
        typeMapping.put("array", "MutableList");
        typeMapping.put("list", "MutableList");
        typeMapping.put("map", "MutableMap");
        typeMapping.put("object", "Any");
        typeMapping.put("binary", "MutableList");
        typeMapping.put("date", "Date");
        typeMapping.put("date-time", "Date");
        typeMapping.put("Date", "Date");
        typeMapping.put("DateTime", "Date");

        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("Date", "java.util.Date");

        // Add special reserved words
        reservedWords.add("null");
        reservedWords.add("request");

        operationTemplateFiles.put("requestBuilder.kt.mustache", ".kt");
        supportingFiles.add(new SupportingFile("testng.mustache", "", "testng.xml"));
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        if (objs == null) return super.postProcessOperations(objs);

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                for (CodegenParameter param : operation.allParams) {
                    Map<String, Object> allowableValues = param.getAllowableValues();
                    if (allowableValues != null) {
                        param.allowableValuesForEnum = new HashMap<>();
                        for (Map.Entry<String, Object> value : allowableValues.entrySet()) {
                            List<CodegenParameter.Tuple<String, String>> formattedValues = new ArrayList<>();
                            for (String val : (ArrayList<String>)value.getValue()) {
                                formattedValues.add(new CodegenParameter.Tuple<>(this.toEnumVarName(val, "String"), val));
                            }
                            param.allowableValuesForEnum.put("values", formattedValues);
                        }
                    }
                }
            }
        }

        return super.postProcessOperations(objs);
    }

    @Override
    public String getName() { return "purecloudkotlin"; }

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
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        List<Object> models = (List<Object>) objs.get("models");

        // Iterate through models
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // Iterate through properties
            for (CodegenProperty cp : cm.allVars) {
                // Enums with values only
                // Fixes issue in a model where there is a name clash with a method
                if (cp.name.equals("isPaid")) {
                    cp.description = "isPaid";
                }

                if (cp.baseType.equals("MutableList")) {
                    cp.defaultValue = "mutableListOf()";
                }

                if (cp.isEnum && cp.allowableValues != null) {
                    Object valuesObject = cp.allowableValues.get("values");
                    if (valuesObject != null) {
                        ArrayList valuesArray = (ArrayList) valuesObject;
                        if (valuesArray.get(0) instanceof Integer) {
                            // Integer enum type
                            System.out.println("Adding 'OUTDATEDSDKVERSION(-1)' to " + cm.name + "." + cp.name + "Enum");
                            valuesArray.add(0, -1);
                            Object enumVarsObject = cp.allowableValues.get("enumVars");
                            ArrayList enumVarsArray = (ArrayList) enumVarsObject;
                            HashMap<String, String> newItem = new HashMap<String, String>();
                            newItem.put("name", "OUTDATEDSDKVERSION");
                            newItem.put("value", toEnumValue("-1", "Integer"));
                            enumVarsArray.add(0, newItem);
                        } else {
                            // String enum type
                            System.out.println("Adding 'OUTDATEDSDKVERSION(\"OutdatedSdkVersion\")' to " + cm.name + "." + cp.name + "Enum");
                            valuesArray.add(0, "OutdatedSdkVersion");
                            Object enumVarsObject = cp.allowableValues.get("enumVars");
                            ArrayList enumVarsArray = (ArrayList) enumVarsObject;
                            HashMap<String, String> newItem = new HashMap<String, String>();
                            newItem.put("name", "OUTDATEDSDKVERSION");
                            newItem.put("value", toEnumValue("OutdatedSdkVersion", "String"));
                            enumVarsArray.add(0, newItem);
                        }
                    }
                }
            }

        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if (property.description != null) {
            // [DEVENGAGE-345] Escape /* in description - this interferes with javadoc comments and annotations - transforms /* -> \\/\\*
            property.description = property.description.replaceAll("/\\*","\\\\\\\\/\\\\\\\\*");
        }
    }

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

        codegenModel.isPagedResource = true;

        for (String s : Arrays.asList("pageSize","pageNumber","total","selfUri","firstUri","previousUri","nextUri","lastUri","pageCount", "entities")) {
            if (!codegenModel.allVars.stream().anyMatch(var -> var.name.equals(s))) {
                codegenModel.isPagedResource = false;
                break;
            }
        }

        // Fixes issue where some variables have a doller sign in the name and this causes an issue with the JsonProperty annotation
        codegenModel.allVars.forEach(codegenProperty -> codegenProperty.baseName = codegenProperty.baseName.replace("$", ""));

        if (codegenModel.isPagedResource) {
            // Get reference to entities property
            Optional<CodegenProperty> entitiesProperty = codegenModel.allVars.stream().filter(var -> var.name.equals("entities")).findFirst();
            if (!entitiesProperty.isPresent()) {
                codegenModel.isPagedResource = false;
                return codegenModel;
            }

            System.out.println(codegenModel.classname + " implements PagedResource");

            // datatypeWithEnum has the correct type including generics. complexType drops them.
            // E.g. datatypeWithEnum=Map<Object, String> and complexType=Map
            codegenModel.pagedResourceType = entitiesProperty.get().datatypeWithEnum;
            if (codegenModel.pagedResourceType.startsWith("MutableList<")) {
                codegenModel.pagedResourceType = codegenModel.pagedResourceType.substring(12,codegenModel.pagedResourceType.length() - 1);
                System.out.println("  pagedResourceType truncated to " + codegenModel.pagedResourceType);
            }
            codegenModel.imports.add("PagedResource");
        }

        return codegenModel;
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof MapProperty) {
            // API-2916 default values for Map properties cause unexpected issues for PUT requests
            return "null";
        } else {
            String defaultValue = super.toDefaultValue(p);
            return defaultValue;
        }
    }
}
