package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Info;
import io.swagger.models.License;
import io.swagger.models.Swagger;
import io.swagger.models.Scheme;
import io.swagger.models.Operation;
import io.swagger.models.HttpMethod;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.io.File;
import java.util.*;

public class k6Codegen extends DefaultCodegen implements CodegenConfig {

    static class Parameter {
        String key;
        Object value;

        public Parameter(String key, Object value) {
            this.key = key;
            this.value = value;
        }

        @Override
        public int hashCode() {
            return key.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null || getClass() != obj.getClass())
                return false;
            Parameter p = (Parameter) obj;
            return key.equals(p.key) && value.equals((String) p.value);
        }
    }

    static class HTTPBody {
        List<Parameter> parameters;

        public HTTPBody(List<Parameter> parameters) {
            this.parameters = parameters;
        }

        public boolean hasMoreParameters() {
            return parameters.size() > 1;
        }
    }

    static class HTTPParameters {
        @Nullable
        String auth;
        @Nullable
        List<Parameter> cookies;
        @Nullable
        List<Parameter> headers;
        @Nullable
        List<Parameter> jar;
        @Nullable
        Integer redirects;
        @Nullable
        List<Parameter> tags;
        @Nullable
        Integer timeout;
        @Nullable
        String compression;
        @Nullable
        String responseType;

        public HTTPParameters(@Nullable String auth, @Nullable List<Parameter> cookies,
                @Nullable List<Parameter> headers, @Nullable List<Parameter> jar, @Nullable Integer redirects,
                @Nullable List<Parameter> tags, @Nullable Integer timeout, @Nullable String compression,
                @Nullable String responseType) {
            this.auth = auth;
            this.cookies = cookies;
            this.headers = headers;
            this.jar = jar;
            this.redirects = redirects;
            this.tags = tags;
            this.timeout = timeout;
            this.compression = compression;
            this.responseType = responseType;
        }

        public HTTPParameters() {
        }

        public boolean hasMoreCookies() {
            return cookies.size() > 1;
        }

        public boolean hasMoreHeaders() {
            return headers.size() > 1;
        }

        public boolean hasMoreJar() {
            return jar.size() > 1;
        }

        public boolean hasMoreTags() {
            return tags.size() > 1;
        }
    }

    static class HTTPRequest {
        String method;
        String path;
        @Nullable
        List<String> query;
        @Nullable
        HTTPBody body;
        @Nullable
        HTTPParameters params;

        public HTTPRequest(String method, String path, @Nullable List<String> query, @Nullable HTTPBody body,
                @Nullable HTTPParameters params) {
            this.method = method;
            this.path = path;
            this.query = query;
            this.body = body;
            this.params = params;
        }
    }

    static public class HTTPRequestGroup {
        String groupName;
        List<HTTPRequest> requests;

        public HTTPRequestGroup(String groupName, List<HTTPRequest> requests) {
            this.groupName = groupName;
            this.requests = requests;
        }
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(JavascriptClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    public static final String MODULE_NAME = "moduleName";
    public static final String PROJECT_DESCRIPTION = "projectDescription";
    public static final String PROJECT_VERSION = "projectVersion";
    public static final String BASE_URL = "baseURL";

    public static final String LOAD_TEST_DATA_FROM_FILE = "loadTestDataFromFile";
    public static final String TEST_DATA_FILE = "testDataFile";
    public static final String PRESERVE_LEADING_PARAM_CHAR = "preserveLeadingParamChar";
    static final Collection<String> INVOKER_PKG_SUPPORTING_FILES = Arrays.asList("script.mustache", "README.mustache");
    static final String[][] JAVASCRIPT_SUPPORTING_FILES = new String[][] {
            new String[] { "script.mustache", "script.js" },
            new String[] { "README.mustache", "README.md" }
    };

    protected String projectName;
    protected String moduleName;
    protected String projectDescription;
    protected String projectVersion;
    protected String licenseName;

    protected String invokerPackage;
    protected String sourceFolder = "";
    protected String localVariablePrefix = "";
    private String modelPropertyNaming = "camelCase";

    protected boolean loadTestDataFromFile = false;
    protected File testDataFile = null;
    protected boolean preserveLeadingParamChar = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "k6";
    }

    @Override
    public String getHelp() {
        return "Generates k6 script.";
    }

    @Override
    public void processOpts() {
        embeddedTemplateDir = templateDir = "k6";

        super.processOpts();

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName(((String) additionalProperties.get(PROJECT_NAME)));
        }
        if (additionalProperties.containsKey(MODULE_NAME)) {
            setModuleName(((String) additionalProperties.get(MODULE_NAME)));
        }
        if (additionalProperties.containsKey(PROJECT_DESCRIPTION)) {
            setProjectDescription(((String) additionalProperties.get(PROJECT_DESCRIPTION)));
        }
        if (additionalProperties.containsKey(PROJECT_VERSION)) {
            setProjectVersion(((String) additionalProperties.get(PROJECT_VERSION)));
        }
        if (additionalProperties.containsKey(CodegenConstants.LICENSE_NAME)) {
            setLicenseName(((String) additionalProperties.get(CodegenConstants.LICENSE_NAME)));
        }
        if (additionalProperties.containsKey(CodegenConstants.LOCAL_VARIABLE_PREFIX)) {
            setLocalVariablePrefix((String) additionalProperties.get(CodegenConstants.LOCAL_VARIABLE_PREFIX));
        }
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }
        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        }
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }
        boolean preserveLeadingParamChar = convertPropertyToBooleanAndWriteBack(PRESERVE_LEADING_PARAM_CHAR);
        this.setPreserveLeadingParamChar(preserveLeadingParamChar);
    }

    private String trimBrackets(String s) {
        if (s != null) {
            int beginIdx = s.charAt(0) == '[' ? 1 : 0;
            int endIdx = s.length();
            if (s.charAt(endIdx - 1) == ']')
                endIdx--;
            return s.substring(beginIdx, endIdx);
        }
        return null;
    }

    private String getModelledType(String dataType) {
        return "module:" + (StringUtils.isEmpty(invokerPackage) ? "" : (invokerPackage + "/"))
                + (StringUtils.isEmpty(modelPackage) ? "" : (modelPackage + "/")) + dataType;
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (StringUtils.isBlank(projectName) && info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = sanitizeName(dashize(info.getTitle()));
            }
            if (StringUtils.isBlank(projectVersion)) {
                // when projectVersion is not specified, use info.version
                projectVersion = escapeUnsafeCharacters(escapeQuotationMark(info.getVersion()));
            }
            if (projectDescription == null) {
                // when projectDescription is not specified, use info.description
                projectDescription = sanitizeName(info.getDescription());
            }

            // when licenceName is not specified, use info.license
            if (additionalProperties.get(CodegenConstants.LICENSE_NAME) == null && info.getLicense() != null) {
                License license = info.getLicense();
                licenseName = license.getName();
            }
        }

        // default values
        if (StringUtils.isBlank(projectName)) {
            projectName = "swagger-k6-client";
        }
        if (StringUtils.isBlank(moduleName)) {
            moduleName = camelize(underscore(projectName));
        }
        if (StringUtils.isBlank(projectVersion)) {
            projectVersion = "1.0.0";
        }
        if (projectDescription == null) {
            projectDescription = "Client library of " + projectName;
        }
        if (StringUtils.isBlank(licenseName)) {
            licenseName = "Unlicense";
        }

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(MODULE_NAME, moduleName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(CodegenConstants.LICENSE_NAME, licenseName);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.LOCAL_VARIABLE_PREFIX, localVariablePrefix);
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);

        String scheme = swagger.getSchemes().contains(Scheme.HTTPS) ? "https://" : "http://";
        additionalProperties.put(BASE_URL, scheme + swagger.getHost() + swagger.getBasePath());

        // NOTE: This code sample shows the usage of k6 related data structures.
        /*
         final HTTPParameters anythingParams = new HTTPParameters(null, null, new
         ArrayList() {
         {
         add(new Parameter("Accept", "application/json"));
         }
         }, null, null, null, null, null, null);

         List<HTTPRequest> anything = new ArrayList<>();
         anything.add(new HTTPRequest("get", "/anything"));
         anything.add(new HTTPRequest("post", "/anything", anythingParams));

         final HTTPParameters authParams = new HTTPParameters("digest", null, new
         ArrayList() {
         {
         add(new Parameter("Accept", "application/json"));
         }
         }, null, null, null, null, null, null);

         List<HTTPRequest> auth = new ArrayList<>();
         auth.add(new HTTPRequest("get", "/digest-auth/auth-int/user/passwd",
         authParams));

         List<HTTPRequestGroup> requestGroups = new ArrayList<HTTPRequestGroup>();
         requestGroups.add(new HTTPRequestGroup("Anything", anything));
         requestGroups.add(new HTTPRequestGroup("Auth", auth));

         additionalProperties.put("requestGroups", requestGroups);
        */

        List<HTTPRequestGroup> requestGroups = new ArrayList<HTTPRequestGroup>();
        Set<Parameter> extraParameters = new HashSet<>();

        for (String path : swagger.getPaths().keySet()) {
            List<HTTPRequest> requests = new ArrayList<>();
            for (Map.Entry<HttpMethod, Operation> methodop : swagger.getPath(path).getOperationMap().entrySet()) {
                List<Parameter> httpParams = new ArrayList<>();
                List<String> queryParams = new ArrayList<>();
                List<Parameter> bodyParams = new ArrayList<>();

                for (io.swagger.models.parameters.Parameter p : methodop.getValue().getParameters()) {
                    switch (p.getIn()) {
                        case "header":
                            httpParams.add(new Parameter(p.getName(), "`${" + p.getName() + "}`"));
                            extraParameters.add(new Parameter(p.getName(), p.getName().toUpperCase()));
                            break;
                        case "path":
                        case "query":
                            queryParams.add("`${" + p.getName() + "}`");
                            extraParameters.add(new Parameter(p.getName(), p.getName().toUpperCase()));
                            break;
                        case "body":
                            // TODO: should be further extended to include Schema Definition (Model)
                            bodyParams.add(new Parameter(p.getName(), p.getName()));
                            break;
                        default:
                            break;
                    }
                }

                final HTTPParameters params = new HTTPParameters(null, null, httpParams, null, null, null, null, null,
                        null);

                if (path.contains("/{")) {
                    path = path.replace("/{", "/${");
                }

                requests.add(new HTTPRequest(methodop.getKey().toString().toLowerCase(), path,
                        queryParams.size() > 0 ? queryParams : null,
                        bodyParams.size() > 0 ? new HTTPBody(bodyParams) : null,
                        params.headers.size() > 0 ? params : null));
            }
            requestGroups.add(new HTTPRequestGroup(path, requests));
        }

        additionalProperties.put("requestGroups", requestGroups);
        additionalProperties.put("extra", extraParameters);

        String[][] supportingTemplateFiles = JAVASCRIPT_SUPPORTING_FILES;
        for (String[] supportingTemplateFile : supportingTemplateFiles) {
            String templateFile = supportingTemplateFile[0];
            String folder;
            if (INVOKER_PKG_SUPPORTING_FILES.contains(templateFile))
                // #1150: script.js must be generated to invokerPackage, otherwise
                // nothing works!
                folder = createPath(sourceFolder, invokerPackage);
            else
                folder = "";
            supportingFiles.add(new SupportingFile(templateFile, folder, supportingTemplateFile[1]));
        }
    }

    /**
     * Concatenates an array of path segments into a path string.
     * 
     * @param segments The path segments to concatenate. A segment may contain
     *                 either of the file separator characters '\' or '/'. A segment
     *                 is ignored if it is <code>null</code>, empty or
     *                 &quot;.&quot;.
     * @return A path string using the correct platform-specific file separator
     *         character.
     */
    private String createPath(String... segments) {
        StringBuilder buf = new StringBuilder();
        for (String segment : segments) {
            if (!StringUtils.isEmpty(segment) && !segment.equals(".")) {
                if (buf.length() != 0)
                    buf.append(File.separatorChar);
                buf.append(segment);
            }
        }
        for (int i = 0; i < buf.length(); i++) {
            char c = buf.charAt(i);
            if ((c == '/' || c == '\\') && c != File.separatorChar)
                buf.setCharAt(i, File.separatorChar);
        }
        return buf.toString();
    }

    @Override
    public String apiFileFolder() {
        return createPath(outputFolder, sourceFolder, invokerPackage, apiPackage());
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setLocalVariablePrefix(String localVariablePrefix) {
        this.localVariablePrefix = localVariablePrefix;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setProjectDescription(String projectDescription) {
        this.projectDescription = projectDescription;
    }

    public void setProjectVersion(String projectVersion) {
        this.projectVersion = projectVersion;
    }

    public void setLicenseName(String licenseName) {
        this.licenseName = licenseName;
    }

    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) || "PascalCase".equals(naming)
                || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" + naming
                    + "'. Must be 'original', 'camelCase', " + "'PascalCase' or 'snake_case'");
        }
    }

    public void setPreserveLeadingParamChar(boolean preserveLeadingParamChar) {
        this.preserveLeadingParamChar = preserveLeadingParamChar;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }
}
