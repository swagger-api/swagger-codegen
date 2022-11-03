package io.swagger.codegen.languages;

import io.swagger.codegen.*;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ScalaCaskCodegen extends AbstractScalaCodegen implements CodegenConfig {

    static String env(String key, String defaultValue) {
        final String value = System.getenv().get(key);
        return value == null || value.isEmpty() ? defaultValue : value;
    }

    static boolean debug = Boolean.parseBoolean(env("DEBUG", "false"));

    protected String groupId = env("GROUP_ID", "io.swagger");
    protected String artifactId = env("ARTIFACT_ID", "caskgen");
    protected String basePackage = env("PACKAGE", groupId);
    protected String artifactVersion = "1.0.0";
    protected String appName = env("APP_NAME", "Cask App");
    protected String infoEmail = env("INFO_EMAIL", "apiteam@swagger.io");

    static String ApiServiceTemplate = "apiService.mustache";

    static String pretty(CodegenResponse response) {
        final Map<String, Object> map = new HashMap<String, Object>();
        map.put("class", "CodegenResponse");
        map.put("code", response.code);
        map.put("message", response.message);
        map.put("headers", response.headers);
        map.put("dataType", response.dataType);
        map.put("baseType", response.baseType);
        map.put("containerType", response.containerType);
        map.put("simpleType", response.simpleType);
        map.put("primitiveType", response.primitiveType);
        map.put("isListContainer", response.isListContainer);
        map.put("isMapContainer", response.isMapContainer);
        map.put("schema", response.schema);
        map.put("jsonSchema", response.jsonSchema);
        map.put("examples", response.examples);
        map.put("vendorExtensions", formatMap(response.vendorExtensions));
        return formatMap(map);
    }

    static String pretty(CodegenParameter response) {
        final Map<String, Object> map = new HashMap<String, Object>();
        map.put("class", "CodegenParameter");
        map.put("baseName", response.baseName);
        map.put("paramName", response.paramName);
        map.put("baseType", response.baseType);
        map.put("dataType", response.dataType);
        map.put("required", response.required);
        map.put("datatypeWithEnum", response.datatypeWithEnum);
        map.put("dataFormat", response.dataFormat);
        map.put("collectionFormat", response.collectionFormat);
        map.put("enumName", response.enumName);
        map.put("isFile", response.isFile);
        map.put("isEnum", response.isEnum);
        map.put("isConstEnumParam", response.isConstEnumParam);
        map.put("hasValidation", response.hasValidation);
        map.put("multipleOf", response.multipleOf);
        map.put("isListContainer", response.isListContainer);
        map.put("isMapContainer", response.isMapContainer);
        map.put("jsonSchema", response.jsonSchema);
        map.put("vendorExtensions", formatMap(response.vendorExtensions));
        if (response.items == null) {
            map.put("items", "null");
        } else {
            map.put("items", pretty(response.items));
        }
        return formatMap(map);
    }

    static String pretty(CodegenOperation response) {
        final Map<String, Object> map = new HashMap<String, Object>();
        map.put("class", "CodegenOperation");
        map.put("baseName", response.baseName);
        map.put("imports", String.join(";", response.imports));
        map.put("tags", String.join(";", response.tags.stream().map(i -> i.getName()).collect(Collectors.toList())));
        map.put("operationIdOriginal", response.operationIdOriginal);
        map.put("returnType", response.returnType);
        map.put("httpMethod", response.httpMethod);
        map.put("returnBaseType", response.returnBaseType);
        map.put("returnContainer", response.returnContainer);
        map.put("summary", response.summary);
        map.put("isListContainer", response.isListContainer);
        map.put("isMapContainer", response.isMapContainer);
        map.put("unescapedNotes", response.unescapedNotes);
        map.put("notes", response.notes);
        map.put("defaultResponse", response.defaultResponse);
        map.put("discriminator", response.discriminator);
        map.put("vendorExtensions", formatMap(response.vendorExtensions));
        return formatMap(map);
    }

    static String pretty(CodegenProperty response) {
        final Map<String, Object> map = new HashMap<String, Object>();
        map.put("class", "CodegenProperty");
        map.put("baseName", response.baseName);
        map.put("complexType", response.complexType);
        map.put("baseType", response.baseType);
        map.put("title", response.title);
        map.put("required", response.required);
        map.put("datatypeWithEnum", response.datatypeWithEnum);
        map.put("dataFormat", response.dataFormat);
        map.put("enumName", response.enumName);
        map.put("isListContainer", response.isListContainer);
        map.put("isMapContainer", response.isMapContainer);
        map.put("jsonSchema", response.jsonSchema);
        map.put("vendorExtensions", formatMap(response.vendorExtensions));
        return formatMap(map);
    }

    static String formatMap(Map<?, ?> map) {
        StringBuilder mapAsString = new StringBuilder("{");
        for (Object key : map.keySet().stream().sorted().collect(Collectors.toList())) {
            mapAsString.append(key + " -- " + map.get(key) + ",\n");
        }
        if (mapAsString.length() > 1) {
            mapAsString.delete(mapAsString.length() - 2, mapAsString.length());
        }
        mapAsString.append("}");
        return mapAsString.toString();
    }

    static String inComment(String text) {
        // these maps are to insert some context in our templates.
        // This 'debug' flag is a convenient way to short-circuit that. Just too bad that 'text' can't easily
        // be made lazy. Hey ho ... Java ¯\_(ツ)_/¯ ?
        if (!debug) {
            return "";
        }
        return text.trim().isEmpty() ? "" : "\n/** " + text + " */\n";
    }

    public ScalaCaskCodegen() {
        super();

        outputFolder = "generated-code/cask";
        modelTemplateFiles.put("model.mustache", ".scala");

        apiTemplateFiles.put("apiRoutes.mustache", ".scala");
        apiTemplateFiles.put(ApiServiceTemplate, "Service.scala");

        embeddedTemplateDir = templateDir = "cask";
        apiPackage = basePackage + ".server.api";
        modelPackage = basePackage + ".server.model";

        final String apiPath = "src/main/scala/" + apiPackage.replace('.', '/');
        final String modelPath = "src/main/scala/" + modelPackage.replace('.', '/');


        setReservedWordsLowerCase(
                Arrays.asList(
                        "abstract", "continue", "for", "new", "switch", "assert",
                        "default", "if", "package", "synchronized", "boolean", "do", "goto", "private",
                        "this", "break", "double", "implements", "protected", "throw", "byte", "else",
                        "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
                        "catch", "extends", "int", "short", "try", "char", "final", "interface", "static",
                        "void", "class", "finally", "long", "strictfp", "volatile", "const", "float",
                        "native", "super", "while", "type")
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList("double",
                        "Int",
                        "Long",
                        "Float",
                        "Double",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "List",
                        "Set",
                        "Map")
        );

        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "String");

        additionalProperties.put("appName", appName);
        additionalProperties.put("appDescription", "A cask service");
        additionalProperties.put("infoUrl", "http://swagger.io");
        additionalProperties.put("infoEmail", infoEmail);
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, basePackage);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("appPackage.mustache", sourceFolder, "package.scala"));
        supportingFiles.add(new SupportingFile("apiPackage.mustache", apiPath, "package.scala"));
        supportingFiles.add(new SupportingFile("modelPackage.mustache", modelPath, "package.scala"));
        supportingFiles.add(new SupportingFile("app.mustache", sourceFolder, "App.scala"));
        supportingFiles.add(new SupportingFile("project/build.properties", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.sbt", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("serviceResponse.mustache", apiPath, "ServiceResponse.scala"));


        instantiationTypes.put("array", "Seq");
        instantiationTypes.put("map", "Map");

        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "scala.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.time.LocalDate as Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "Map");
        importMapping.put("HashMap", "Map");
        importMapping.put("Array", "Seq");
        importMapping.put("ArrayList", "Seq");
        importMapping.put("List", "Seq");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        name = sanitizeName(name);
        return camelize(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        String fn = toApiFilename(tag);
        if (templateName.equals(ApiServiceTemplate)) {
            return apiFileFolder() + '/' + fn + suffix;
        } else {
            return apiFileFolder() + '/' + fn + "Routes" + suffix;
        }
    }


    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "cask";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala server application with Cask.";
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        List<Map<String, Object>> operations = getOperations(objs);
        for (int i = 0; i < operations.size(); i++) {
            operations.get(i).put("hasMore", i < operations.size() - 1);
        }
        objs.put("operations", operations);
        return super.postProcessSupportingFileData(objs);
    }

    // thanks FlaskConnectionCodeGen
    private static List<Map<String, Object>> getOperations(Map<String, Object> objs) {
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<Map<String, Object>> apis = (List<Map<String, Object>>) apiInfo.get("apis");
        for (Map<String, Object> api : apis) {
            Map<String, Object> operations = (Map<String, Object>) api.get("operations");
            result.add(operations);
        }
        return result;
    }

    static boolean consumesMimetype(CodegenOperation op, String mimetype) {
        // people don't always/often specify the 'consumes' property, so we assume true when
        // the optional 'consumes' is null or empty
        boolean defaultRetValue = true;

        final List<Map<String, String>> consumes = op.consumes;
        if (consumes != null) {
            for (Map<String, String> c : consumes) {
                final String mt = c.get("mediaType");
                if (mt.equalsIgnoreCase(mimetype)) {
                    return true;
                }
            }
            return false;
        } else {
            return defaultRetValue;
        }
    }

    public static class ParamPart {
        final CodegenParameter param;
        final String name;
        final boolean isParam;

        // flag for if there are more path parts
        boolean hasMore;
        // flag for if there are more path parts which are parameters
        boolean hasMoreParams;

        final String conversion;

        public ParamPart(String name, CodegenParameter param) {
            this.name = name;
            this.param = param;
            this.isParam = param != null;
            this.hasMore = true;
            this.conversion = !isParam || param.isString ? "" : ".to" + param.dataType;
        }
    }


    static String capitalise(String p) {
        if (p.length() < 2) {
            return p.toUpperCase();
        } else {
            String first = "" + p.charAt(0);
            return first.toUpperCase() + p.substring(1);
        }
    }


    /**
     * Cask will compile but 'initialize' can throw a route overlap exception:
     * <p>
     * {{{
     * Routes overlap with wildcards: get /user/logout, get /user/:username, get /user/login
     * }}}
     * <p>
     * Note: The same error persists even if the suffixes are unique:
     * {{{
     * Routes overlap with wildcards: get /user/logout/3, get /user/:username/1, get /user/login/2
     * }}}
     * <p>
     * To fix this, we need to identify and resolve conflicts in our generated code.
     * <p>
     * # How do we identify conflicts?
     * 1. group routes by their non-param prefixes.
     * <p>
     * 2. add an "x-annotation" vendor extension for operations
     * <p>
     * 3. add a list of "RouteGroups" which can manually delegate as per below
     * <p>
     * <p>
     * # How do we resolve conflicts?
     * <p>
     * We leave out the cask route annotation on the conflicting operations, e.g. :
     * {{{
     * //conflict: @cask.get("/user/:username")
     * def getUserByName(username: String, request: cask.Request) = ...
     * }}}
     * <p>
     * and we introduce a new discriminator function to "manually" call those conflicts:
     * {{{
     *
     * @cask.get("/user", subpath = true)
     * def userRouteDescriminator(request: cask.Request) = {
     * request.remainingPathSegments match {
     * case Seq("logout") => logoutUser(request)
     * case Seq("login") => loginUser(request)
     * case Seq(param) => getUserByName(param, request)
     * }
     * }
     * }}}
     */
    public static class OperationGroup {
        List<CodegenOperation> operations = new ArrayList<>();
        final String pathPrefix;
        final String httpMethod;
        final String caskAnnotation;
        final String methodName;

        // TODO - multiple operations may have the same query params, so we'll need to somehow merge them (and take the right type)
        public boolean hasGroupQueryParams() {
            return operations.stream().flatMap(op -> op.queryParams.stream()).count() > 0;
        }

        public List<CodegenParameter> getGroupQueryParams() {
            List<CodegenParameter> list = operations.stream().flatMap(op -> op.queryParams.stream()).map(p -> {
                        final CodegenParameter copy = p.copy();
                        copy.vendorExtensions.put("x-default-value", defaultValue(p));
                        copy.required = false; // all our query params are optional for our work-around as it's a super-set of a few different routes
                        copy.dataType = asScalaDataType(copy);
                        copy.defaultValue = defaultValue(copy);
                        return copy;
                    }
            ).collect(Collectors.toList());

            if (!list.isEmpty()) {
                CodegenParameter last = list.get(list.size() - 1);
                // we change our query params for the work-around
                list.forEach(p -> {
                    p.hasMore = !p.equals(last);
                });
            }
            return list;
        }

        @Override
        public String toString() {
            List<String> ops = operations.stream().map(o -> o.path + "\n").collect(Collectors.toList());
            return httpMethod + " " + pathPrefix + " w/ " + operations.size() + " operations:\n" + String.join("", ops);
        }

        public OperationGroup(String httpMethod, String pathPrefix) {
            this.httpMethod = httpMethod;
            this.pathPrefix = pathPrefix;
            caskAnnotation = "@cask." + httpMethod.toLowerCase();

            List<String> stripped = Arrays.stream(pathPrefix.split("/", -1))
                    .map(p -> capitalise(p)).collect(Collectors.toList());

            methodName = "routeWorkAroundFor" + capitalise(httpMethod) + "" + String.join("", stripped);
        }

        public void add(CodegenOperation op) {
            if (!op.path.startsWith(pathPrefix)) {
                throw new IllegalArgumentException("inconsistent path: " + pathPrefix);
            }
            if (!op.httpMethod.equals(httpMethod)) {
                throw new IllegalArgumentException("inconsistent method: " + httpMethod);
            }

            final List<ParamPart> pathParts = new ArrayList<>();
            final List<String> parts = Arrays.stream(op.path.substring(pathPrefix.length()).split("/", -1)).filter(p -> !p.isEmpty()).collect(Collectors.toList());
            for (int i = 0; i < parts.size(); i++) {
                String p = parts.get(i);
                ParamPart pp = hasBrackets(p) ? new ParamPart(chompBrackets(p), pathParamForName(op, chompBrackets(p))) : new ParamPart(p, null);
                pathParts.add(pp);
            }

            List<ParamPart> paramPathParts = pathParts.stream().filter(p -> p.isParam).collect(Collectors.toList());
            if (!paramPathParts.isEmpty()) {
                final String lastParamName = paramPathParts.get(paramPathParts.size() - 1).name;
                paramPathParts.forEach(p -> p.hasMoreParams = !p.name.equals(lastParamName));
            }
            if (!pathParts.isEmpty()) {
                pathParts.get(pathParts.size() - 1).hasMore = false;
            }

            op.vendorExtensions.put("x-path-remaining", pathParts);
            op.vendorExtensions.put("x-has-path-remaining", !paramPathParts.isEmpty());
            operations.add(op);
        }

        public boolean contains(CodegenOperation op) {
            return operations.contains(op);
        }

        public void updateAnnotations() {
            operations.forEach(op -> {
                String annotation = op.vendorExtensions.get("x-annotation").toString();
                String conflicts = String.join(", ", operations.stream().map(o -> o.path).collect(Collectors.toList()));
                op.vendorExtensions.put("x-annotation", "// conflicts with [" + conflicts + "] after" + pathPrefix + ", ignoring " + annotation);
            });
            operations = operations.stream().sorted((a, b) -> a.pathParams.size() - b.pathParams.size()).collect(Collectors.toList());
        }
    }

    public static class QueryParam {

    }

    static List<OperationGroup> group(List<CodegenOperation> operationList) {
        Map<String, OperationGroup> groupedByPrefix = new HashMap<>();
        operationList.forEach(op -> {
            String prefix = nonParamPathPrefix(op);
            String key = op.httpMethod + " " + prefix;
            if (!op.pathParams.isEmpty()) {
                final OperationGroup group = groupedByPrefix.getOrDefault(key, new OperationGroup(op.httpMethod, prefix));
                group.add(op);
                groupedByPrefix.put(key, group);
            }
        });
        return groupedByPrefix.values().stream().collect(Collectors.toList());
    }

    static String nonParamPathPrefix(CodegenOperation op) {
        if (op.pathParams.isEmpty()) {
            return op.path;
        }

        final String firstParam = op.pathParams.stream().findFirst().get().paramName;
        final int i = op.path.indexOf(firstParam);
        final String path = chompSuffix(op.path.substring(0, i - 1), "/");
        return path;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        final Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        final List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        List<OperationGroup> groups = group(operationList);
        operationList.forEach((op) -> {
            for (final OperationGroup group : groups) {
                // for the usage/call site
                final String scalaPath = pathWithBracketPlaceholdersRemovedAndXPathIndexAdded(op);
                op.vendorExtensions.put("x-cask-path", scalaPath);

                final String annotation = "@cask." + op.httpMethod.toLowerCase();
                op.vendorExtensions.put("x-annotation", annotation);
                if (!group.contains(op)) {
                    if (op.path.startsWith(group.pathPrefix) && op.httpMethod.equalsIgnoreCase(group.httpMethod)) {
                        group.add(op);
                    }
                }
            }
        });

        List<OperationGroup> trimmed = groups.stream().filter(g -> g.operations.size() > 1).map(g -> {
            g.updateAnnotations();
            return g;
        }).collect(Collectors.toList());
        objs.put("route-groups", trimmed);

        operationList.forEach((op) -> postProcessOperation(op));
        return objs;
    }

    private static void postProcessOperation(CodegenOperation op) {
        // force http method to lower case
        op.httpMethod = op.httpMethod.toLowerCase();

        op.vendorExtensions.put("x-debug", inComment(pretty(op)));
        op.allParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));
        op.bodyParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));
        op.pathParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));
        op.queryParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));
        op.headerParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));
        op.formParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));
        op.requiredParams.forEach(p -> p.vendorExtensions.put("x-debug", inComment(pretty(p))));


        /** Put in 'x-consumes-json' and 'x-consumes-xml' */
        op.vendorExtensions.put("x-consumes-json", consumesMimetype(op, "application/json"));
        op.vendorExtensions.put("x-consumes-xml", consumesMimetype(op, "application/xml"));

        op.bodyParams.stream().filter((b) -> b.isBodyParam).forEach((p) -> {
            p.vendorExtensions.put("x-consumes-json", consumesMimetype(op, "application/json"));
            p.vendorExtensions.put("x-consumes-xml", consumesMimetype(op, "application/xml"));
        });

        /** put in 'x-container-type' to help with unmarshalling from json */
        op.allParams.forEach((p) -> p.vendorExtensions.put("x-container-type", containerType(p.dataType)));
        op.bodyParams.forEach((p) -> p.vendorExtensions.put("x-container-type", containerType(p.dataType)));

        final String paramList = String.join(", ", op.allParams.stream().map((p) -> p.paramName).collect(Collectors.toList()));
        op.vendorExtensions.put("x-param-list", paramList);

        final Stream<String> typed = op.allParams.stream().map((p) -> p.paramName + " : " + p.dataType);
        final String typedParamList = String.join(", ", typed.collect(Collectors.toList()));
        op.vendorExtensions.put("x-param-list-typed", typedParamList);


        // for the declaration site
        op.vendorExtensions.put("x-cask-path-typed", routeArgs(op));
        op.vendorExtensions.put("x-query-args", queryArgs(op));

        op.vendorExtensions.put("x-response-type", enrichResponseType(op));
        String responseDebug = String.join("\n\n - - - - - - -\n\n", op.responses.stream().map(r -> inComment(pretty(r))).collect(Collectors.toList()));
        op.vendorExtensions.put("x-responses", responseDebug);

    }

    /**
     * The Service methods are decoupled from http Responses -- they return a typed 'ServiceResponse'.
     * This method tries to fill int the ServiceResponse type parameter
     * For example:
     * {{{
     * "200": {
     * "description": "successful operation",
     * "schema": {
     * "type": "string"
     * }
     * },
     * "400": {
     * "description": "Invalid username/password supplied"
     * }
     * }}}
     *
     * @param op
     * @return
     */
    private static String enrichResponseType(CodegenOperation op) {
        if (op.returnType != null && !op.returnType.isEmpty()) {
            return "ServiceResponse[" + op.returnType + "]";
        }
        Optional<CodegenResponse> successResponse = op.responses.stream().filter((r) -> r.code.startsWith("2")).findFirst();
        if (successResponse.isPresent()) {
            CodegenResponse r = successResponse.get();

            return "ServiceResponse[Unit] /**" +
                    "containerType='" + r.containerType + "'\n" +
                    "baseType='" + r.baseType + "'\n" +
                    "dataType='" + r.dataType + "'\n" +
                    "simpleType='" + r.simpleType + "'\n" +
                    "jsonSchema='" + r.jsonSchema + "'\n" +
                    "schema='" + r.schema + "'\n" +
                    "*/";
        } else {
            return "ServiceResponse[Unit]";
        }
    }


    /**
     * Cask path params use the :pathParam syntax rather than the {pathParam} syntax
     *
     * @param op
     * @return
     */
    private static String pathWithBracketPlaceholdersRemovedAndXPathIndexAdded(CodegenOperation op) {
        String[] items = op.path.split("/", -1);
        String scalaPath = "";
        for (int i = 0; i < items.length; ++i) {
            final String nextPart = hasBrackets(items[i]) ? ":" + chompBrackets(items[i]) : items[i];
            if (i != items.length - 1) {
                scalaPath = scalaPath + nextPart + "/";
            } else {
                scalaPath = scalaPath + nextPart;
            }
        }
        return scalaPath;
    }

    private static CodegenParameter pathParamForName(CodegenOperation op, String pathParam) {
        final CodegenParameter param = op.pathParams.stream().filter(p -> p.paramName.equals(pathParam)).findFirst().get();
        if (param == null) {
            throw new RuntimeException("Bug: path param " + pathParam + " not found");
        }
        return param;
    }

    /**
     * The path placeholders as well as query parameters
     *
     * @param op the codegen operations
     * @return a list of both the path and query parameters as typed arguments (e.g. "aPathArg : Int, request: cask.Request, aQueryArg : Option[Long]")
     */
    private static String routeArgs(CodegenOperation op) {
        final Stream<String> pathParamNames = Arrays.stream(op.path.split("/", -1)).filter(p -> hasBrackets(p)).map(p -> {
            final CodegenParameter param = pathParamForName(op, chompBrackets(p));
            param.vendorExtensions.put("x-debug", inComment(pretty(param)));
            return param.paramName + " : " + asScalaDataType(param);
        });


        final List<String> pathList = pathParamNames.collect(Collectors.toList());

        // we always include the cask request
        pathList.add("request: cask.Request");

        final Stream<String> queryParams = op.queryParams.stream().map(p -> {
            p.vendorExtensions.put("x-default-value", defaultValue(p));
            return p.paramName + " : " + asScalaDataType(p);
        });
        pathList.addAll(queryParams.collect(Collectors.toList()));
        return pathList.isEmpty() ? "" : (String.join(", ", pathList));
    }

    private static String defaultValue(CodegenParameter p) {
        if (!p.required && !(p.isListContainer || p.isMapContainer)) {
            return "None";
        }
        return defaultValueNonOption(p);
    }

    private static String defaultValueNonOption(CodegenParameter p) {
        if (p.isListContainer) {
            return "Nil";
        }
        if (p.isMapContainer) {
            return "Map.empty";
        }
        if (p.isNumber) {
            return "0";
        }
        if (p.isString) {
            return "\"\"";
        }
        if (p.isBoolean) {
            return "false";
        }
        if (p.isUuid) {
            return "java.util.UUID.randomUUID()";
        }
        return p.defaultValue;
    }

    private static String queryArgs(final CodegenOperation op) {
        final List<String> list = op.queryParams.stream().map(p -> p.paramName).collect(Collectors.toList());
        final String prefix = list.isEmpty() ? "" : ", ";
        return prefix + String.join(", ", list);
    }

    private static String asScalaDataType(final CodegenParameter param) {
        String dataType = param.dataType;
        if (dataType.startsWith("List[")) {
            dataType = dataType.replace("List[", "Seq[");
        } else if (!param.required) {
            dataType = "Option[" + param.dataType + "]";
        }
        return dataType;
    }

    private static String chompBrackets(String str) {
        return str.replace("{", "").replace("}", "");
    }

    private static String chompSuffix(String str, String suffix) {
        return str.endsWith(suffix) ? chompSuffix(str.substring(0, str.length() - suffix.length()), suffix) : str;
    }

    private static boolean hasBrackets(String str) {
        return str.matches("^\\{(.*)\\}$");
    }

    static String containerType(String dataType) {
        String fixedForList = dataType.replaceAll(".*\\[(.*)\\]", "$1");
        // do we have to fix map?
        return fixedForList;
    }

}
