package io.swagger.codegen.languages;

import com.google.common.base.Predicate;

import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import io.swagger.codegen.*;
import io.swagger.models.Swagger;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.parameters.HeaderParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;

import javax.annotation.Nullable;
import java.util.*;
import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Swift2Codegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";
    public static final String PROJECT_NAMESPACE = "projectNamespace";
    public static final String SWIFT_USE_API_NAMESPACE = "swiftUseApiNamespace";
    protected String projectName = "SwaggerClient";
    protected boolean unwrapRequired;
    protected boolean swiftUseApiNamespace;
    protected String[] responseAs = new String[0];
    protected String sourceFolder = "src";
    private static final Pattern PATH_PARAM_PATTERN = Pattern.compile("\\{[a-zA-Z_]+\\}");

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "swift2";
    }

    @Override
    public String getHelp() {
        return "Generates a swift client library.";
    }

    public Swift2Codegen() {
        super();
        outputFolder = "generated-code" + File.separator + "swift";
        modelTemplateFiles.put("model.mustache", ".swift");
        apiTemplateFiles.put("api.mustache", ".swift");
        embeddedTemplateDir = templateDir = "swift";
        apiPackage = File.separator + "APIs";
        modelPackage = File.separator + "Models";

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                    "Int",
                    "Float",
                    "Double",
                    "Bool",
                    "Void",
                    "String",
                    "Character",
                    "AnyObject")
                );
        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                    "NSDate",
                    "NSURL", // for file
                    "Array",
                    "Dictionary",
                    "Set",
                    "Any",
                    "Empty",
                    "AnyObject")
                );
        setReservedWordsLowerCase(
                Arrays.asList(
                    "class", "break", "as", "associativity", "deinit", "case", "dynamicType", "convenience", "enum", "continue",
                    "false", "dynamic", "extension", "default", "is", "didSet", "func", "do", "nil", "final", "import", "else",
                    "self", "get", "init", "fallthrough", "Self", "infix", "internal", "for", "super", "inout", "let", "if",
                    "true", "lazy", "operator", "in", "COLUMN", "left", "private", "return", "FILE", "mutating", "protocol",
                    "switch", "FUNCTION", "none", "public", "where", "LINE", "nonmutating", "static", "while", "optional",
                    "struct", "override", "subscript", "postfix", "typealias", "precedence", "var", "prefix", "Protocol",
                    "required", "right", "set", "Type", "unowned", "weak")
                );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("array", "Array");
        typeMapping.put("list", "Array");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("date", "NSDate");
        typeMapping.put("Date", "NSDate");
        typeMapping.put("dateTime", "NSDate");
        typeMapping.put("DateTime", "NSDate");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("char", "Character");
        typeMapping.put("short", "Int");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Int");
        typeMapping.put("integer", "Int");
        typeMapping.put("Integer", "Int");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "Double");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "AnyObject");
        typeMapping.put("file", "NSURL");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "String");

        importMapping = new HashMap<String, String>();

        cliOptions.add(new CliOption(PROJECT_NAME, "Project name"));
        cliOptions.add(new CliOption(SWIFT_USE_API_NAMESPACE, "Flag to make all the Model classes inner-class of the api main class"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        // Setup project name
        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName((String) additionalProperties.get(PROJECT_NAME));
        } else {
            additionalProperties.put(PROJECT_NAME, projectName);
        }
        String originalSourceFolder = new String(sourceFolder);
        sourceFolder = sourceFolder + File.separator + projectName;

        if (additionalProperties.containsKey(SWIFT_USE_API_NAMESPACE)) {
            swiftUseApiNamespace = Boolean.parseBoolean(String.valueOf(additionalProperties.get(SWIFT_USE_API_NAMESPACE)));
        }

        additionalProperties.put(SWIFT_USE_API_NAMESPACE, swiftUseApiNamespace);
        if (swiftUseApiNamespace) {
            additionalProperties.put(PROJECT_NAMESPACE, toApiName(projectName));
        }
        supportingFiles.add(new SupportingFile("APIHelper.mustache", originalSourceFolder + File.separator + "common", "APIHelper.swift"));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;  // add an underscore to the name
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "[String:" + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type) || defaultIncludes.contains(type))
                return type;
        } else
            type = swaggerType;
        return toModelName(type);
    }

    /**
     * Output the proper model name (capitalized)
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(String name) {
        name = sanitizeName(name);  // FIXME parameter should not be assigned. Also declare it as "final"

        if (!StringUtils.isEmpty(modelNameSuffix)) { // set model suffix
            name = name + "_" + modelNameSuffix;
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) { // set model prefix
            name = modelNamePrefix + "_" + name;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model" + name;
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            String modelName = "Model" + name; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return name + "DataModel";
    }

    /**
     * Return the capitalized file name of the model
     *
     * @param name the model name
     * @return the file name of the model
     */
    @Override
    public String toModelFilename(String name) {
        String modelName = toModelName(name);
        if (swiftUseApiNamespace) {
            modelName = String.valueOf(additionalProperties.get(PROJECT_NAMESPACE)) + modelName;
        }
        return modelName;
    }

    @Override
    public String toDefaultValue(Property p) {
        // nil
        return null;
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return "[String:" + inner + "]";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return "[" + inner + "]";
        }
        return null;
    }

    @Override
    public CodegenProperty fromProperty(String name, Property p) {
        CodegenProperty codegenProperty = super.fromProperty(name, p);
        if (codegenProperty.isEnum) {
            List<Map<String, String>> swiftEnums = new ArrayList<Map<String, String>>();
            List<String> values = (List<String>) codegenProperty.allowableValues.get("values");
            for (String value : values) {
                Map<String, String> map = new HashMap<String, String>();
                map.put("enum", toSwiftyEnumName(value));
                map.put("raw", value);
                swiftEnums.add(map);
            }
            codegenProperty.allowableValues.put("values", swiftEnums);
            codegenProperty.datatypeWithEnum =
                StringUtils.left(codegenProperty.datatypeWithEnum, codegenProperty.datatypeWithEnum.length() - "Enum".length());
            // Ensure that the enum type doesn't match a reserved word or
            // the variable name doesn't match the generated enum type or the
            // Swift compiler will generate an error
            if (isReservedWord(codegenProperty.datatypeWithEnum) ||
                    name.equals(codegenProperty.datatypeWithEnum)) {
                codegenProperty.datatypeWithEnum = escapeReservedWord(codegenProperty.datatypeWithEnum);
                }
            if (String.valueOf(codegenProperty.datatypeWithEnum.charAt(0)).equals("_")) {
                codegenProperty.datatypeWithEnum = codegenProperty.datatypeWithEnum + "Enum";
            }
        }
        return codegenProperty;
    }

    @SuppressWarnings("static-method")
    public String toSwiftyEnumName(String value) {

        // Prevent from breaking properly cased identifier
        if (value.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            return value;
        }
        char[] separators = {'-', '_', ' '};
        return WordUtils.capitalizeFully(StringUtils.lowerCase(value), separators).replaceAll("[-_ ]", "");
    }


    @Override
    public String toApiName(String name) {
        if(name.length() == 0 || name.equals("Default"))
            return (String) additionalProperties.get(PROJECT_NAME) + "API";
        return initialCaps(name) + "API";
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = camelize(sanitizeName(operationId), true); 

        // throw exception if method name is empty. This should not happen but keep the check just in case
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(("call_" + operationId), true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        return operationId;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        path = normalizePath(path); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        return super.fromOperation(path, httpMethod, operation, definitions, swagger);
    }

    private static String normalizePath(String path) {
        StringBuilder builder = new StringBuilder();

        int cursor = 0;
        Matcher matcher = PATH_PARAM_PATTERN.matcher(path);
        boolean found = matcher.find();
        while (found) {
            String stringBeforeMatch = path.substring(cursor, matcher.start());
            builder.append(stringBeforeMatch);

            String group = matcher.group().substring(1, matcher.group().length() - 1);
            group = camelize(group, true);
            builder
                .append("{")
                .append(group)
                .append("}");

            cursor = matcher.end();
            found = matcher.find();
        }

        String stringAfterMatch = path.substring(cursor);
        builder.append(stringAfterMatch);

        return builder.toString();
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setUnwrapRequired(boolean unwrapRequired) {
        this.unwrapRequired = unwrapRequired;
    }

    public void setResponseAs(String[] responseAs) {
        this.responseAs = responseAs;
    }
}
