package io.swagger.codegen.languages;

import java.io.File;
import java.util.*;

import io.swagger.codegen.*;
import io.swagger.models.properties.*;

public class AdaCodegen extends AbstractAdaCodegen implements CodegenConfig {
    public static final String USER_INFO_PATH = "userInfoPath";
    protected String packageName = "swagger";
    protected String userInfoPath = "/var/www/html/";
    protected List<Map<String, Object>> orderedModels;
    protected Map<String, List<String>> modelDepends;

    public AdaCodegen() {
        super();

        modelNameSuffix = "_Type";
        orderedModels = new ArrayList<Map<String, Object>>();
        modelDepends = new HashMap<String, List<String>>();
        embeddedTemplateDir = templateDir = "Ada";

        cliOptions.add(new CliOption(USER_INFO_PATH, "Path to the user and group files"));

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME, "Ada package name (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.MODEL_PACKAGE, "Ada package for models (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "Ada package for apis (convention: name.space.api).",
                this.apiPackage);

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("integer", "boolean", "Integer", "Character", "Boolean", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "Swagger.Date");
        typeMapping.put("DateTime", "Swagger.Datetime");
        typeMapping.put("string", "Swagger.UString");
        typeMapping.put("integer", "Integer");
        typeMapping.put("long", "Swagger.Long");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("array", "Swagger.Vector");
        typeMapping.put("map", "ASF.Types.Map");
        typeMapping.put("object", "Swagger.Object");
        typeMapping.put("number", "Swagger.Number");
        typeMapping.put("UUID", "Swagger.String");
        typeMapping.put("file", "Swagger.Http_Content_Type");
        typeMapping.put("binary", "Swagger.Binary");

        super.importMapping = new HashMap<String, String>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("HttpContent", "#include \"HttpContent.h\"");
        importMapping.put("Object", "#include \"Object.h\"");
        importMapping.put("utility::string_t", "#include <cpprest/details/basic_types.h>");
        importMapping.put("utility::datetime", "#include <cpprest/details/basic_types.h>");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CONFIG;
    }

    @Override
    public String getName() {
        return "Ada";
    }

    @Override
    public String getHelp() {
        return "Generates an Ada server implementation";
    }

    protected void addOption(String key, String description, String defaultValue) {
        CliOption option = new CliOption(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    public String toFilename(String name) {
        return name.replace(".", "-").toLowerCase();
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);
        }
        String serverPrefix = "src" + File.separator + "server" + File.separator + toFilename(modelPackage);
        String clientPrefix = "src" + File.separator + "client" + File.separator + toFilename(modelPackage);
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, clientPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, clientPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, serverPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, serverPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("client-spec.mustache", null, clientPrefix + "-clients.ads"));
        supportingFiles.add(new SupportingFile("client-body.mustache", null, clientPrefix + "-clients.adb"));
        supportingFiles.add(new SupportingFile("server-spec.mustache", null, serverPrefix + "-servers.ads"));
        supportingFiles.add(new SupportingFile("server-body.mustache", null, serverPrefix + "-servers.adb"));
        if (additionalProperties.containsKey(USER_INFO_PATH)) {
            userInfoPath = additionalProperties.get(USER_INFO_PATH).toString();
        }
        // String title = swagger.getInfo().getTitle();
        supportingFiles.add(new SupportingFile("gnat-project.mustache", "", "project.gpr"));

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("package", this.modelPackage);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/model/" + modelPackage().replace('.', File.separatorChar);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "p_" + name; // add an underscore to the name
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     *         `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);

        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getTypeDeclaration(inner) + "_Vectors.Vector";
        }
        if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "<utility::string_t, " + getTypeDeclaration(inner) + ">";
        }
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        //  LOGGER.info("Swagger type " + swaggerType);
        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }
        String modelType = toModelName(swaggerType);
        if (p instanceof StringProperty || p instanceof DateProperty
                || p instanceof DateTimeProperty || p instanceof FileProperty
                || languageSpecificPrimitives.contains(modelType)) {
            return modelType;
        }

        return modelPackage + ".Models." + modelType;
    }

    /**
     * Overrides postProcessParameter to add a vendor extension "x-is-model-type".
     * This boolean indicates that the parameter comes from the model package.
     *
     * @param parameter CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter){
        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        parameter.vendorExtensions.put("x-is-model-type", parameter.dataType.startsWith(modelPackage));
    }

    /**
     * @brief Post process the media types (produces and consumes) for Ada code generator.
     *
     * For each media type, add a adaMediaType member that gives the Ada enum constant
     * for the corresponding type.
     *
     * @param types the list of media types.
     */
    protected int postProcessMediaTypes(List<Map<String, String>> types) {
        int count = 0;
        if (types != null) {
            for (Map<String, String> media : types) {
                String mt = media.get("mediaType");
                if (mt != null) {
                    mt = mt.replace('/', '_');
                    media.put("adaMediaType", mt.toUpperCase());
                    count++;
                }
            }
        }
        return count;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation op1 : operationList) {
            if (postProcessMediaTypes(op1.produces) == 1) {
                op1.vendorExtensions.put("x-has-uniq-produces", Boolean.TRUE);
            } else {
                op1.vendorExtensions.put("x-has-uniq-produces", Boolean.FALSE);
            }
            if (postProcessMediaTypes(op1.consumes) == 1) {
                op1.vendorExtensions.put("x-has-uniq-consumes", Boolean.TRUE);
            } else {
                op1.vendorExtensions.put("x-has-uniq-consumes", Boolean.FALSE);
            }
            if (op1.notes.length() > 0) {
                op1.vendorExtensions.put("x-has-notes", Boolean.TRUE);
            } else {
                op1.vendorExtensions.put("x-has-notes", Boolean.FALSE);
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // Collect the model dependencies.
        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> model : models) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                List<String> d = new ArrayList<String>();
                for (CodegenProperty p : m.allVars) {
                    if (!p.isString && !p.isPrimitiveType && !p.isContainer && !p.isInteger) {
                        if (!d.contains(p.datatype)) {
                            // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                            d.add(p.datatype);
                        }
                    }
                }
                modelDepends.put(m.name, d);
                orderedModels.add(model);
            }
        }

        // Sort the models according to dependencies so that model that depend
        // on others appear at end of the list.
        final Map<String, List<String>> deps = modelDepends;
        Collections.sort(orderedModels, new Comparator<Map<String, Object>>() {
            @Override
            public int compare(Map<String, Object> lhs, Map<String, Object> rhs) {
                Object v = lhs.get("model");
                String lhsName = ((CodegenModel) v).name;
                v = rhs.get("model");
                String rhsName = ((CodegenModel) v).name;
                List<String> lhsList = deps.get(lhsName);
                List<String> rhsList = deps.get(rhsName);
                if (lhsList == rhsList) {
                    // LOGGER.info("First compare " + lhsName + "<" + rhsName);
                    return lhsName.compareTo(rhsName);
                }
                // Put models without dependencies first.
                if (lhsList == null) {
                    // LOGGER.info("  Empty " + lhsName + ", no check " + rhsName);
                    return -1;
                }
                if (rhsList == null) {
                    // LOGGER.info("  No check " + lhsName + ", empty " + rhsName);
                    return 1;
                }
                // Put models that depend on another after.
                if (lhsList.contains(rhsName)) {
                    // LOGGER.info("  LSH " + lhsName + " uses " + rhsName);
                    return 1;
                }
                if (rhsList.contains(lhsName)) {
                    // LOGGER.info("  RHS " + rhsName + " uses " + lhsName);
                    return -1;
                }
                // Put models with less dependencies first.
                if (lhsList.size() < rhsList.size()) {
                    // LOGGER.info("  LSH size " + lhsName + " < RHS size " + rhsName);
                    return -1;
                }
                if (lhsList.size() > rhsList.size()) {
                    // LOGGER.info("  LSH size " + lhsName + " > RHS size " + rhsName);
                    return 1;
                }
                // Sort models on their name.
                // LOGGER.info("Compare " + lhsName + "<" + rhsName);
                return lhsName.compareTo(rhsName);
            }
        });
        /* for (Map<String, Object> model : orderedModels) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                LOGGER.info("Order: " + m.name);
            }
        }*/
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs.put("orderedModels", orderedModels);
        return super.postProcessSupportingFileData(objs);
    }
}
