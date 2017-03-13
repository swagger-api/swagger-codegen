package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class PureCloudJavaClientCodegenSdkV1 extends JavaClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudJavaClientCodegenSdkV1.class);

    public PureCloudJavaClientCodegenSdkV1() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "Java";

        // Custom mappings for swagger type -> java type
        importMapping.put("LocalDateTime", "org.joda.time.LocalDateTime");
        importMapping.put("IPagedResource", "com.mypurecloud.sdk.v2.IPagedResource");

        // Add special reserved words
        reservedWords.add("null");
    }



    @Override
    public String getName() { return "purecloudjavasdkv1"; }

    @Override
    /**
     * Get the value of x-purecloud-method-name, or use default C# behavior if blank.
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
                System.out.println("Using operation ID property " + OPERATION_ID_PROPERTY_NAME + " (" + operationId +  ") for path " + path);
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
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

        codegenModel.isPagedResource = true;

        for (String s : Arrays.asList("pageSize","pageNumber","total","selfUri","firstUri","previousUri","nextUri","lastUri","pageCount", "entities")) {
            if (!codegenModel.allVars.stream().anyMatch(var -> var.name.equals(s))) {
                codegenModel.isPagedResource = false;
                break;
            }
        }

        if (codegenModel.isPagedResource) {
            // Get reference to entities property
            Optional<CodegenProperty> entitiesProperty = codegenModel.allVars.stream().filter(var -> var.name.equals("entities")).findFirst();
            if (!entitiesProperty.isPresent()) {
                codegenModel.isPagedResource = false;
                return codegenModel;
            }

            System.out.println(codegenModel.classname + " implements IPagedResource");
            codegenModel.pagedResourceType = entitiesProperty.get().complexType;
            codegenModel.imports.add("IPagedResource");
        }

        return codegenModel;
    }
}
