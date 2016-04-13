package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.*;

import org.apache.commons.lang.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GoClientCodegen extends DefaultCodegen implements CodegenConfig {
    static Logger LOGGER = LoggerFactory.getLogger(GoClientCodegen.class);

    protected String packageName = "swagger";
    protected String packageVersion = "1.0.0";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "go";
    }

    public String getHelp() {
        return "Generates a Go client library (beta).";
    }

    public GoClientCodegen() {
        super();

        // We use GoCodegenParameter instead of CodegenParameter because we wish
        // to add an extra field (exportParamName).
        CodegenModelFactory.setTypeMapping(CodegenModelType.PARAMETER, GoCodegenParameter.class);

        outputFolder = "generated-code/go";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");
        templateDir = "go";

        setReservedWordsLowerCase(
            Arrays.asList(
                "break", "default", "func", "interface", "select",
                "case", "defer", "go", "map", "struct",
                "chan", "else", "goto", "package", "switch",
                "const", "fallthrough", "if", "range", "type",
                "continue", "for", "import", "return", "var", "error")
                // Added "error" as it's used so frequently that it may as well be a keyword
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                    "map",
                    "array")
                );

        languageSpecificPrimitives = new HashSet<String>(
            Arrays.asList(
                "string",
                "bool",
                "uint",
                "uint32",
                "uint64",
                "int",
                "int32",
                "int64",
                "float32",
                "float64",
                "complex64",
                "complex128",
                "rune",
                "byte")
            );

        instantiationTypes.clear();
        /*instantiationTypes.put("array", "GoArray");
        instantiationTypes.put("map", "GoMap");*/

        typeMapping.clear();
        typeMapping.put("integer", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float32");
        typeMapping.put("float", "float32");
        typeMapping.put("double", "float64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "string");
        typeMapping.put("date", "time.Time");
        typeMapping.put("DateTime", "time.Time");
        typeMapping.put("password", "string");
        typeMapping.put("File", "*os.File");
        typeMapping.put("file", "*os.File");
        // map binary to string as a workaround
        // the correct solution is to use []byte
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");

        importMapping = new HashMap<String, String>();
        importMapping.put("time.Time", "time");
        importMapping.put("*os.File", "os");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Go package name (convention: lowercase).")
                .defaultValue("swagger"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Go package version.")
                .defaultValue("1.0.0"));

    }

    @Override
    public void processOpts() {
        //super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("configuration.mustache", packageName, "Configuration.go"));
    }

    @Override
    public String escapeReservedWord(String name)
    {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that Go treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.

        // FIXME: This should also really be a customizable option
        return camelize(name) + '_';
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + packageName;
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + packageName;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // camelize (lower first character) the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word or word starting with number, append _
        if(isReservedWord(name) || name.matches("^\\d.*"))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        // params should be lowerCamelCase. E.g. "person Person", instead of
        // "Person Person".
        //
        // REVISIT: Actually, for idiomatic go, the param name should
        // really should just be a letter, e.g. "p Person"), but we'll get
        // around to that some other time... Maybe.
        return camelize(toVarName(name), true);
    }

    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if(p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[]" + getTypeDeclaration(inner);
        }
        else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "[string]" + getTypeDeclaration(inner);
        }
        //return super.getTypeDeclaration(p);

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }

        if(typeMapping.containsValue(swaggerType)) {
            return swaggerType;
        }

        if(languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }

        return camelize(swaggerType, false);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if(typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if(languageSpecificPrimitives.contains(type))
                return (type);
        }
        else
            type = swaggerType;
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(operationId);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion (e.g. PUT => Put)
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase());
        }

        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        if (imports == null)
            return objs;

        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(apiPackage()))
                iterator.remove();
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final String prefix = modelPackage();
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix))
                iterator.remove();
        }
        return objs;
    }


    /**
     * Overrides fromParameter to provide an extra field, "exportParamName". This
     * is useful when paramName starts with a lowercase letter, but we need that
     * param to be exportable (starting with an uppercsae letter).
     *
     * @param param Swagger parameter object
     * @param imports set of imports for library/package/module
     * @return Instance of GoCodegenParameter.
     */
    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {

        CodegenParameter cp = super.fromParameter(param, imports);
        GoCodegenParameter gcp = (GoCodegenParameter) cp;

        char firstChar = gcp.paramName.charAt(0);

        if (Character.isUpperCase(firstChar)) {
            // First char is already uppercase, just use paramName.
            gcp.exportParamName = gcp.paramName;
            return gcp;
        }

        // It's a lowercase first char, let's convert it to uppercase
        StringBuffer sb = new StringBuffer(gcp.paramName);
        sb.setCharAt(0, Character.toUpperCase(firstChar));
        gcp.exportParamName = sb.toString();

        return gcp;
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
            && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    /**
     * GoCodegenParameter extends CodegenParameter to add a field {@link #exportParamName}
     * which is guaranteed to be exported (that is to say, the first letter is upper-case).
     */
    public static class GoCodegenParameter extends CodegenParameter {

        public String exportParamName;

        public GoCodegenParameter() {

        }

        // REVISIT (neilotoole): do we need to provide equals() and hashCode()?
        // The parent class (CodegenParameter) does not implement those methods.


        @Override
        public CodegenParameter copy() {
            GoCodegenParameter output = new GoCodegenParameter();
            output.exportParamName = this.exportParamName;

            output.isFile = this.isFile;
            output.notFile = this.notFile;
            output.hasMore = this.hasMore;
            output.isContainer = this.isContainer;
            output.secondaryParam = this.secondaryParam;
            output.baseName = this.baseName;
            output.paramName = this.paramName;
            output.dataType = this.dataType;
            output.datatypeWithEnum = this.datatypeWithEnum;
            output.collectionFormat = this.collectionFormat;
            output.isCollectionFormatMulti = this.isCollectionFormatMulti;
            output.description = this.description;
            output.baseType = this.baseType;
            output.isFormParam = this.isFormParam;
            output.isQueryParam = this.isQueryParam;
            output.isPathParam = this.isPathParam;
            output.isHeaderParam = this.isHeaderParam;
            output.isCookieParam = this.isCookieParam;
            output.isBodyParam = this.isBodyParam;
            output.required = this.required;
            output.maximum = this.maximum;
            output.exclusiveMaximum = this.exclusiveMaximum;
            output.minimum = this.minimum;
            output.exclusiveMinimum = this.exclusiveMinimum;
            output.maxLength = this.maxLength;
            output.minLength = this.minLength;
            output.pattern = this.pattern;
            output.maxItems = this.maxItems;
            output.minItems = this.minItems;
            output.uniqueItems = this.uniqueItems;
            output.multipleOf = this.multipleOf;
            output.jsonSchema = this.jsonSchema;
            output.defaultValue = this.defaultValue;
            output.example = this.example;
            output.isEnum = this.isEnum;
            if (this._enum != null) {
                output._enum = new ArrayList<String>(this._enum);
            }
            if (this.allowableValues != null) {
                output.allowableValues = new HashMap<String, Object>(this.allowableValues);
            }
            if (this.items != null) {
                output.items = this.items;
            }
            output.vendorExtensions = this.vendorExtensions;
            output.isBinary = this.isBinary;
            output.isByteArray = this.isByteArray;
            output.isString = this.isString;
            output.isInteger = this.isInteger;
            output.isLong = this.isLong;
            output.isDouble = this.isDouble;
            output.isFloat = this.isFloat;
            output.isBoolean = this.isBoolean;
            output.isDate = this.isDate;
            output.isDateTime = this.isDateTime;
            output.isListContainer = this.isListContainer;
            output.isMapContainer = this.isMapContainer;

            return output;
        }
    }
}
