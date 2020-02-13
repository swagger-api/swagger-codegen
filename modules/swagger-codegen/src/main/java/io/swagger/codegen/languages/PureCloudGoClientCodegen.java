package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Operation;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

public class PureCloudGoClientCodegen extends GoClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudGoClientCodegen.class);

    public PureCloudGoClientCodegen() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "go";

        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));


        // Go type for arbitrary objects
        // Mainly used for API types of Map<string, Object>, which are objects with additional properties of type object
        typeMapping.put("object", "map[string]interface{}");

        typeMapping.put("LocalDateTime", "time.Time");
    }

    @Override
    public String getName() {
        return "purecloudgo";
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

    public String apiFileFolder() {
        return (outputFolder + "/platformclientv2").replace('/', File.separatorChar);
    }

    public String modelFileFolder() {
        return (outputFolder + "/platformclientv2").replace('/', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/docs").replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/docs").replace('/', File.separatorChar);
    }

    @Override
    public String toModelFilename(String name) {
        return super.toModelFilename(name).replaceAll("_", "").toLowerCase();
    }

    @Override
    public String toApiFilename(String name) {
        return super.toApiFilename(name).replaceAll("_", "").toLowerCase();
    }

    @Override
    public String escapeReservedWord(String name) {
        return "Var" + name;
    }

    @Override
    public String toVarName(String name) {
        // replace non-alphanumeric with underscore
        name = name.replaceAll("[^a-zA-Z0-9]", "_");

        // camelize (lower first character) the variable name
        // pet_id => PetId
        name = camelize(name);

        // Full strip
        name = name.replaceAll("[^a-zA-Z0-9]", "");

        // Escape invalid names
        if (isReservedWord(name) || name.matches("^\\d.*"))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[]" + getTypeDeclaration(inner);
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            if (inner.getType() == "object") {
                // Prevent `map[string]map[string]interface{}` when map value type is object
                return getSwaggerType(p) + "[string]interface{}";
            } else {
                return getSwaggerType(p) + "[string]" + getTypeDeclaration(inner);
            }
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }

        if (typeMapping.containsValue(swaggerType)) {
            return swaggerType;
        }

        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }

        return toModelName(swaggerType);
    }

    private HashMap<String, CodegenModel> codegenModelMap = new HashMap<>();

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);


        // Index all CodegenModels by model name.
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                codegenModelMap.put(cm.classname, cm);
            }
        }
        for (CodegenModel cm : codegenModelMap.values()) {
            markRecursiveProperties(cm, new CodegenProperty[0]);
        }
        return objs;
    }

    private void markRecursiveProperties(CodegenModel cm, CodegenProperty[] lineage) {
        if (cm == null) return;
        String pad = StringUtils.leftPad("", lineage.length * 2, " ");
        for (CodegenProperty cp : cm.vars) {
            String lineageString = "0";
            for (CodegenProperty l : lineage) {
                lineageString += " > " + l.datatype;
                if (l.datatype.equalsIgnoreCase(cp.datatype) && !l.datatype.startsWith("*")) {
                    l.datatype = "*" + l.datatype;
                }
            }
            if ((cp.isPrimitiveType == null || !cp.isPrimitiveType) && codegenModelMap.containsKey(cp.datatype) && !Arrays.stream(lineage).anyMatch(l -> l.datatype.equalsIgnoreCase((cp.datatype)))) {
                ArrayList<CodegenProperty> lineageAL = new ArrayList<>(Arrays.asList(lineage));
                lineageAL.add(cp);
                CodegenProperty[] lineageArray = lineageAL.toArray(new CodegenProperty[0]);
                markRecursiveProperties(codegenModelMap.get(cp.datatype), lineageArray);
            }
        }
    }
}
