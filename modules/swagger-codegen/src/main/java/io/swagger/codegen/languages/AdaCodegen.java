package io.swagger.codegen.languages;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Matcher;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.*;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Response;
import io.swagger.models.Swagger;
import io.swagger.models.properties.*;
import io.swagger.util.Json;

public class AdaCodegen extends AbstractAdaCodegen implements CodegenConfig {
    protected String packageName = "swagger";
    protected String projectName = "Swagger";
    protected List<Map<String, Object>> orderedModels;
    protected Map<String, List<String>> modelDepends;
    protected Map<String, String> nullableTypeMapping;
    protected int scopeIndex = 0;
    protected HashMap<String, String> operationsScopes;

    public AdaCodegen() {
        super();

        modelNameSuffix = "_Type";
        orderedModels = new ArrayList<Map<String, Object>>();
        modelDepends = new HashMap<String, List<String>>();
        embeddedTemplateDir = templateDir = "Ada";

        // CLI options
        addOption(CodegenConstants.PROJECT_NAME, "GNAT project name",
                  this.projectName);
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
        typeMapping.put("map", "Swagger.Map");
        typeMapping.put("object", "Swagger.Object");
        typeMapping.put("number", "Swagger.Number");
        typeMapping.put("UUID", "Swagger.UString");
        typeMapping.put("file", "Swagger.Http_Content_Type");
        typeMapping.put("binary", "Swagger.Binary");

        nullableTypeMapping = new HashMap<String, String>();
        nullableTypeMapping.put("date", "Swagger.Nullable_Date");
        nullableTypeMapping.put("DateTime", "Swagger.Nullable_Date");
        nullableTypeMapping.put("string", "Swagger.Nullable_UString");
        nullableTypeMapping.put("integer", "Swagger.Nullable_Integer");
        nullableTypeMapping.put("long", "Swagger.Nullable_Long");
        nullableTypeMapping.put("boolean", "Swagger.Nullable_Boolean");
        nullableTypeMapping.put("object", "Swagger.Object");

        super.importMapping = new HashMap<String, String>();
        operationsScopes = new HashMap<String, String>();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "ada";
    }

    @Override
    public String getHelp() {
        return "Generates an Ada client implementation (beta).";
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
        String srcPrefix = "src" + File.separator;
        String serverPrefix = srcPrefix + "server" + File.separator + toFilename(modelPackage);
        String clientPrefix = srcPrefix + "client" + File.separator + toFilename(modelPackage);
        String implPrefix = srcPrefix + toFilename(modelPackage);
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, clientPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, clientPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, serverPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, serverPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("client-spec.mustache", null, clientPrefix + "-clients.ads"));
        supportingFiles.add(new SupportingFile("client-body.mustache", null, clientPrefix + "-clients.adb"));
        supportingFiles.add(new SupportingFile("server-skeleton-spec.mustache", null, serverPrefix + "-skeletons.ads"));
        supportingFiles.add(new SupportingFile("server-skeleton-body.mustache", null, serverPrefix + "-skeletons.adb"));
        supportingFiles.add(new SupportingFile("server-spec.mustache", null, implPrefix + "-servers.ads"));
        supportingFiles.add(new SupportingFile("server-body.mustache", null, implPrefix + "-servers.adb"));

        // String title = swagger.getInfo().getTitle();
        supportingFiles.add(new SupportingFile("swagger.mustache", "web" + File.separator + "swagger", "swagger.json"));

        if (additionalProperties.containsKey(CodegenConstants.PROJECT_NAME)) {
            projectName = (String) additionalProperties.get(CodegenConstants.PROJECT_NAME);
        } else {
            // default: set project based on package name
            // e.g. petstore.api (package name) => petstore_api (project name)
            projectName = packageName.replaceAll("\\.", "_");
        }
        String configBaseName = modelPackage.toLowerCase();
        supportingFiles.add(new SupportingFile("gnat-project.mustache", "", projectName + ".gpr"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("config.gpr", "", "config.gpr"));
        supportingFiles.add(new SupportingFile("server-properties.mustache", "", configBaseName + ".properties"));

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("package", this.modelPackage);
        additionalProperties.put("packageConfig", configBaseName);
        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);

        String names[] = this.modelPackage.split("\\.");
        String pkgName = names[0];
        additionalProperties.put("packageLevel1", pkgName);
        supportingFiles.add(new SupportingFile("package-spec-level1.mustache", null,
                            "src" + File.separator + names[0].toLowerCase() + ".ads"));
        if (names.length > 1) {
            String fileName = names[0].toLowerCase() + "-" + names[1].toLowerCase() + ".ads";
            pkgName = names[0] + "." + names[1];
            additionalProperties.put("packageLevel2", pkgName);
            supportingFiles.add(new SupportingFile("package-spec-level2.mustache", null,
                                "src" + File.separator + fileName));
        }
        pkgName = this.modelPackage;
        supportingFiles.add(new SupportingFile("server.mustache", null,
                            "src" + File.separator + pkgName.toLowerCase() + "-server.adb"));

        // add lambda for mustache templates
        additionalProperties.put("lambdaAdaComment", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replaceAll("\n$", "");
                writer.write(content.replaceAll("\n", "\n   --  "));
            }
        });
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
//        String result = input.replaceAll("`", "'");
        return input.replace("*/", "*_/").replace("/*", "/_*").replace("-", "_");
    }

    /**
     * Override the Mustache compiler configuration.
     *
     * We don't want to have special characters escaped
     *
     * @param compiler the compiler.
     * @return the compiler to use.
     */
    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        compiler = super.processCompiler(compiler).emptyStringIsFalse(true);

        return compiler.withEscaper(Escapers.NONE);
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

        if (swaggerType != null) {
            swaggerType = swaggerType.replace("-", "_");
        }

        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getTypeDeclaration(inner) + "_Vectors.Vector";
        }
        if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "Swagger." + getTypeDeclaration(inner) + "_Map";
        }
        if (typeMapping.containsKey(swaggerType)) {
            if (p.getRequired()) {
                return typeMapping.get(swaggerType);
            } else {
                return nullableTypeMapping.get(swaggerType);
            }
        }
        //  LOGGER.info("Swagger type " + swaggerType);
        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }
        String modelType = toModelName(swaggerType).replace("-", "_");
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

        boolean isModel = parameter.dataType.startsWith(modelPackage);
        if (!isModel && !parameter.isPrimitiveType && !parameter.isDate
                && !parameter.isString && !parameter.isContainer && !parameter.isFile) {
            isModel = true;
        }
        parameter.vendorExtensions.put("x-is-model-type", isModel);
    }

    /**
     * Post process the media types (produces and consumes) for Ada code generator.
     *
     * For each media type, add a adaMediaType member that gives the Ada enum constant
     * for the corresponding type.
     *
     * @param types the list of media types.
     * @return the number of media types.
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

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation,
                                          Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            Response methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                if (methodResponse.getSchema() != null) {
                    CodegenProperty cm = fromProperty("response", methodResponse.getSchema());
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if(cm.datatype == "HttpContent") {
                        op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }
        return op;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation op1 : operationList) {
            if (op1.summary != null) {
                op1.summary = op1.summary.trim();
            }
            if (op1.notes != null) {
                op1.notes = op1.notes.trim();
            }
            op1.vendorExtensions.put("x-has-uniq-produces", postProcessMediaTypes(op1.produces) == 1);
            op1.vendorExtensions.put("x-has-uniq-consumes", postProcessMediaTypes(op1.consumes) == 1);
            op1.vendorExtensions.put("x-has-notes", op1.notes != null && op1.notes.length() > 0);

            postProcessAuthMethod(op1.authMethods);

            /*
             * Scan the path parameter to construct a x-path-index that tells the index of
             * the path parameter.
             */
            for (CodegenParameter p : op1.pathParams) {
                String path = op1.path;
                int pos = 0;
                int index = 0;
                while (pos >= 0 && pos < path.length()) {
                    int last;
                    pos = path.indexOf('{', pos);
                    if (pos < 0) {
                        break;
                    }
                    pos++;
                    last = path.indexOf('}', pos);
                    index++;
                    if (last < 0) {
                        break;
                    }
                    if (path.substring(pos, last - 1) == p.baseName) {
                        break;
                    }
                    pos = last + 1;
                }
                p.vendorExtensions.put("x-path-index", index);
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
                    boolean isModel = false;
                    CodegenProperty item = p;
                    if (p.isContainer) {
                        item = p.items;
                    }
                    if (item != null && !item.isString && !item.isPrimitiveType && !item.isContainer && !item.isInteger) {
                        if (!d.contains(item.datatype)) {
                            // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                            d.add(item.datatype);
                            isModel = true;
                        }
                    }
                    p.vendorExtensions.put("x-is-model-type", isModel);
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

    /**
     * Collect the scopes to generate a unique identifier for each of them.
     *
     * @param authMethods the auth methods with their scopes.
     */
    private void postProcessAuthMethod(List<CodegenSecurity> authMethods) {
        if (authMethods != null) {
            for (CodegenSecurity authMethod : authMethods) {
                if (authMethod.scopes != null) {
                    for (Map<String, Object> scope : authMethod.scopes) {
                        String name = (String) scope.get("scope");
                        if (operationsScopes.containsKey(name)) {
                            scope.put("ident", operationsScopes.get(name));
                        } else {
                            String ident;
                            if (name.startsWith("https://")) {
                                int pos = name.lastIndexOf('/');
                                ident = name.substring(pos + 1);
                            } else {
                                ident = name;
                            }
                            scopeIndex++;
                            ident = toAdaIdentifier(sanitizeName(ident.replaceAll(":", "_")), "S_");
                            if (operationsScopes.containsValue(ident)) {
                                ident = ident + "_" + scopeIndex;
                            }
                            operationsScopes.put(name, ident);
                            scope.put("ident", ident);
                        }
                    }
                }
                authMethod.name = camelize(sanitizeName(authMethod.name), true);
            }
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs.put("orderedModels", orderedModels);
        Swagger swagger = (Swagger)objs.get("swagger");
        if(swagger != null) {
            String host = swagger.getBasePath();
            try {
                swagger.setHost("SWAGGER_HOST");
                objs.put("swagger-json", Json.pretty().writeValueAsString(swagger).replace("\r\n", "\n"));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
            swagger.setHost(host);
        }

        /**
         * Collect the scopes to generate unique identifiers for each of them.
         */
        List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
        postProcessAuthMethod(authMethods);

        return super.postProcessSupportingFileData(objs);
    }
}
