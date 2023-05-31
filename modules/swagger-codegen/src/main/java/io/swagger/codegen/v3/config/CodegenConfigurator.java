package io.swagger.codegen.v3.config;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import io.swagger.codegen.v3.CliOption;
import io.swagger.codegen.v3.ClientOptInput;
import io.swagger.codegen.v3.ClientOpts;
import io.swagger.codegen.v3.CodegenArgument;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConfigLoader;
import io.swagger.codegen.v3.CodegenConstants;
import io.swagger.codegen.v3.auth.AuthParser;
import io.swagger.codegen.v3.service.HostAccessControl;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import io.swagger.v3.parser.util.ClasspathHelper;
import io.swagger.v3.parser.util.RemoteUrl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * A class that contains all codegen configuration properties a user would want to manipulate.
 * An instance could be created by deserializing a JSON file or being populated from CLI or Maven plugin parameters.
 * It also has a convenience method for creating a ClientOptInput class which is THE object DefaultGenerator.java needs
 * to generate code.
 */
public class CodegenConfigurator implements Serializable {

    public static final Logger LOGGER = LoggerFactory.getLogger(CodegenConfigurator.class);

    private String lang;
    private String inputSpec;
    private boolean flattenInlineSchema;
    private String inputSpecURL;
    private String outputDir;
    private boolean verbose;
    private boolean skipOverwrite;
    private boolean removeOperationIdPrefix;
    private boolean skipInlineModelMatches;
    private String templateDir;
    private String templateVersion;
    private String auth;
    private AuthorizationValue authorizationValue;
    private String apiPackage;
    private String modelPackage;
    private String invokerPackage;
    private String modelNamePrefix;
    private String modelNameSuffix;
    private String groupId;
    private String artifactId;
    private String artifactVersion;
    private String library;
    private String ignoreFileOverride;
    private boolean resolveFully;
    private List<CodegenArgument> codegenArguments = new ArrayList<>();
    private Map<String, String> systemProperties = new HashMap<String, String>();
    private Map<String, String> instantiationTypes = new HashMap<String, String>();
    private Map<String, String> typeMappings = new HashMap<String, String>();
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();
    private Map<String, String> importMappings = new HashMap<String, String>();
    private Set<String> languageSpecificPrimitives = new HashSet<String>();
    private Map<String, String>  reservedWordMappings = new HashMap<String, String>();

    private String gitUserId="GIT_USER_ID";
    private String gitRepoId="GIT_REPO_ID";
    private String gitRepoBaseURL = "https://github.com";
    private String releaseNote="Minor update";
    private String httpUserAgent;

    private final Map<String, Object> dynamicProperties = new HashMap<String, Object>(); //the map that holds the JsonAnySetter/JsonAnyGetter values

    public CodegenConfigurator() {
        this.setOutputDir(".");
    }

    private List<HostAccessControl> allowedAuthHosts = new ArrayList<>();
    private List<HostAccessControl> deniedAuthHosts = new ArrayList<>();

    public List<HostAccessControl> getAllowedAuthHosts() {
        return allowedAuthHosts;
    }

    public void setAllowedAuthHosts(List<HostAccessControl> allowedAuthHosts) {
        this.allowedAuthHosts = allowedAuthHosts;
    }

    public List<HostAccessControl> getDeniedAuthHosts() {
        return deniedAuthHosts;
    }

    public void setDeniedAuthHosts(List<HostAccessControl> deniedAuthHosts) {
        this.deniedAuthHosts = deniedAuthHosts;
    }

    public CodegenConfigurator setLang(String lang) {
        this.lang = lang;
        return this;
    }

    public CodegenConfigurator setInputSpec(String inputSpec) {
        this.inputSpec = inputSpec;
        return this;
    }

    public String getInputSpec() {
        return inputSpec;
    }

    public String getInputSpecURL() {
        return inputSpecURL;
    }

    public CodegenConfigurator setInputSpecURL(String inputSpecURL) {
        this.inputSpecURL = inputSpecURL;
        return this;
    }

    public String getOutputDir() {
        return outputDir;
    }

    public CodegenConfigurator setOutputDir(String outputDir) {
        this.outputDir = toAbsolutePathStr(outputDir);
        return this;
    }

    public String getModelPackage() {
        return modelPackage;
    }

    public CodegenConfigurator setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
        return this;
    }

    public String getModelNamePrefix() {
        return modelNamePrefix;
    }

    public CodegenConfigurator setModelNamePrefix(String prefix) {
        this.modelNamePrefix = prefix;
        return this;
    }

    public boolean getRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    public CodegenConfigurator setRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
        return this;
    }

    public boolean getSkipInlineModelMatches() {
        return skipInlineModelMatches;
    }

    public CodegenConfigurator setSkipInlineModelMatches(boolean skipInlineModelMatches) {
        this.skipInlineModelMatches = skipInlineModelMatches;
        return this;
    }

    public String getModelNameSuffix() {
        return modelNameSuffix;
    }

    public CodegenConfigurator setModelNameSuffix(String suffix) {
        this.modelNameSuffix = suffix;
        return this;
    }

    public boolean isVerbose() {
        return verbose;
    }

    public CodegenConfigurator setVerbose(boolean verbose) {
        this.verbose = verbose;
        return this;
    }

    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }

    public CodegenConfigurator setSkipOverwrite(boolean skipOverwrite) {
        this.skipOverwrite = skipOverwrite;
        return this;
    }

    public String getLang() {
        return lang;
    }

    public String getTemplateDir() {
        return templateDir;
    }

    public CodegenConfigurator setTemplateDir(String templateDir) {
        File f = new File(templateDir);

        // check to see if the folder exists
        if (!(f.exists() && f.isDirectory())) {
            throw new IllegalArgumentException("Template directory " + templateDir + " does not exist.");
        }

        this.templateDir = f.getAbsolutePath();
        return this;
    }

    public String getTemplateVersion() {
        return templateVersion;
    }

    public CodegenConfigurator setTemplateVersion(String templateVersion) {
        this.templateVersion = templateVersion;
        return this;
    }

    public String getAuth() {
        return auth;
    }

    public CodegenConfigurator setAuth(String auth) {
        this.auth = auth;
        return this;
    }

    public AuthorizationValue getAuthorizationValue() {
        return authorizationValue;
    }
    public void setAuthorizationValue(AuthorizationValue authorizationValue) {
        this.authorizationValue = authorizationValue;
    }

    public String getApiPackage() {
        return apiPackage;
    }

    public CodegenConfigurator setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
        return this;
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }

    public CodegenConfigurator setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
        return this;
    }

    public String getGroupId() {
        return groupId;
    }

    public CodegenConfigurator setGroupId(String groupId) {
        this.groupId = groupId;
        return this;
    }

    public String getArtifactId() {
        return artifactId;
    }

    public CodegenConfigurator setArtifactId(String artifactId) {
        this.artifactId = artifactId;
        return this;
    }

    public String getArtifactVersion() {
        return artifactVersion;
    }

    public CodegenConfigurator setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
        return this;
    }

    public Map<String, String> getSystemProperties() {
        return systemProperties;
    }

    public CodegenConfigurator setSystemProperties(Map<String, String> systemProperties) {
        this.systemProperties = systemProperties;
        return this;
    }

    public CodegenConfigurator addSystemProperty(String key, String value) {
        this.systemProperties.put(key, value);
        return this;
    }

    public Map<String, String> getInstantiationTypes() {
        return instantiationTypes;
    }

    public CodegenConfigurator setInstantiationTypes(Map<String, String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
        return this;
    }

    public CodegenConfigurator addInstantiationType(String key, String value) {
        this.instantiationTypes.put(key, value);
        return this;
    }

    public Map<String, String> getTypeMappings() {
        return typeMappings;
    }

    public CodegenConfigurator setTypeMappings(Map<String, String> typeMappings) {
        this.typeMappings = typeMappings;
        return this;
    }

    public CodegenConfigurator addTypeMapping(String key, String value) {
        this.typeMappings.put(key, value);
        return this;
    }

    public Map<String, Object> getAdditionalProperties() {
        return additionalProperties;
    }

    public CodegenConfigurator setAdditionalProperties(Map<String, Object> additionalProperties) {
        this.additionalProperties = additionalProperties;
        return this;
    }

    public CodegenConfigurator addAdditionalProperty(String key, Object value) {
        this.additionalProperties.put(key, value);
        return this;
    }

    public Map<String, String> getImportMappings() {
        return importMappings;
    }

    public CodegenConfigurator setImportMappings(Map<String, String> importMappings) {
        this.importMappings = importMappings;
        return this;
    }

    public CodegenConfigurator addImportMapping(String key, String value) {
        this.importMappings.put(key, value);
        return this;
    }

    public Set<String> getLanguageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    public CodegenConfigurator setLanguageSpecificPrimitives(Set<String> languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
        return this;
    }

    public CodegenConfigurator addLanguageSpecificPrimitive(String value) {
        this.languageSpecificPrimitives.add(value);
        return this;
    }

    public String getLibrary() {
        return library;
    }

    public CodegenConfigurator setLibrary(String library) {
        this.library = library;
        return this;
    }

    public String getGitUserId() {
        return gitUserId;
    }

    public CodegenConfigurator setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
        return this;
    }

    public String getGitRepoId() {
        return gitRepoId;
    }

    public CodegenConfigurator setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
        return this;
    }

    public String getGitRepoBaseURL() {
        return gitRepoBaseURL;
    }

    public CodegenConfigurator setGitRepoBaseURL(String gitRepoBaseURL) {
        this.gitRepoBaseURL = gitRepoBaseURL;
        return this;
    }


    public String getReleaseNote() {
        return releaseNote;
    }

    public CodegenConfigurator setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
        return this;
    }

    public String getHttpUserAgent() {
        return httpUserAgent;
    }

    public CodegenConfigurator setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent= httpUserAgent;
        return this;
    }

    public  Map<String, String> getReservedWordsMappings() {
        return reservedWordMappings;
    }

    public CodegenConfigurator setReservedWordsMappings(Map<String, String> reservedWordsMappings) {
        this.reservedWordMappings = reservedWordsMappings;
        return this;
    }

    public CodegenConfigurator addAdditionalReservedWordMapping(String key, String value) {
        this.reservedWordMappings.put(key, value);
        return this;
    }

    public String getIgnoreFileOverride() {
        return ignoreFileOverride;
    }

    public CodegenConfigurator setIgnoreFileOverride(final String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
        return this;
    }

    public boolean isResolveFully() {
        return resolveFully;
    }

    public CodegenConfigurator setResolveFully(boolean resolveFully) {
        this.resolveFully = resolveFully;
        return this;
    }

    public String loadSpecContent(String location, List<AuthorizationValue> auths) throws Exception{
            location = sanitizeSpecificationUrl(location);
            String data = "";
            if (location.toLowerCase().startsWith("http")) {
                data = RemoteUrl.urlToString(location, auths);
            } else {
                final String fileScheme = "file:";
                Path path;
                if (location.toLowerCase().startsWith(fileScheme)) {
                    path = Paths.get(URI.create(location));
                } else {
                    path = Paths.get(location);
                }
                if (Files.exists(path)) {
                    data = FileUtils.readFileToString(path.toFile(), "UTF-8");
                } else {
                    data = ClasspathHelper.loadFileFromClasspath(location);
                }
            }
            LOGGER.trace("Loaded raw data: {}", data);
            return data;
    }

    private String sanitizeSpecificationUrl(String specificationUrl) {
        return specificationUrl.replaceAll("\\\\","/");
    }

    public ClientOptInput toClientOptInput() {

        Validate.notEmpty(lang, "language must be specified");

        if (StringUtils.isBlank(inputSpec) && StringUtils.isBlank(inputSpecURL)) {
            throw new IllegalArgumentException("input spec or URL must be specified");
        }

        setVerboseFlags();
        setSystemProperties();

        CodegenConfig config = CodegenConfigLoader.forName(lang);
        ClientOptInput input = new ClientOptInput();

        Predicate<URL> urlMatcher = null;
        if (!allowedAuthHosts.isEmpty() || !deniedAuthHosts.isEmpty()) {
            urlMatcher = (url) -> {
                String host = url.getHost();
                // first check denies
                for (HostAccessControl check: deniedAuthHosts) {
                    if (check.isRegex()) {
                        if (host.matches(check.getHost())) {
                            return false;
                        }
                    } else if (check.isEndsWith()){
                        if (host.toLowerCase().endsWith(check.getHost().toLowerCase())){
                            return false;
                        }
                    } else {
                        if (host.equalsIgnoreCase(check.getHost())) {
                            return false;
                        }
                    }
                }
                // then allows
                for (HostAccessControl check: allowedAuthHosts) {
                    if (check.isRegex()) {
                        if (!host.matches(check.getHost())) {
                            return false;
                        }
                    } else if (check.isEndsWith()){
                        if (!host.toLowerCase().endsWith(check.getHost().toLowerCase())){
                            return false;
                        }
                    } else {
                        if (!host.equalsIgnoreCase(check.getHost())) {
                            return false;
                        }
                    }
                }
                return true;
            };
        }

        final List<AuthorizationValue> authorizationValues = AuthParser.parse(auth);
        if (!authorizationValues.isEmpty() && urlMatcher != null) {
            for (AuthorizationValue authVal: authorizationValues) {
                if (authVal.getUrlMatcher() == null) {
                    authVal.setUrlMatcher(urlMatcher);
                }
            }
        }
        if (authorizationValue != null) {
            if (urlMatcher != null) {
                authorizationValue.setUrlMatcher(urlMatcher);
            }
            authorizationValues.add(authorizationValue);
        }

        if (!StringUtils.isBlank(inputSpec)) {
            config.setInputSpec(inputSpec);

            ParseOptions options = buildParseOptions();


            SwaggerParseResult result = new OpenAPIParser().readContents(inputSpec, authorizationValues, options);
            OpenAPI openAPI = result.getOpenAPI();
            if (config.needsUnflattenedSpec()) {
                ParseOptions optionsUnflattened = buildUnflattenedParseOptions();
                SwaggerParseResult resultUnflattened = new OpenAPIParser().readContents(inputSpec, authorizationValues, optionsUnflattened);
                OpenAPI openAPIUnflattened = resultUnflattened.getOpenAPI();
                config.setUnflattenedOpenAPI(openAPIUnflattened);
            }

            input.opts(new ClientOpts())
                    .openAPI(openAPI);

            LOGGER.debug("getClientOptInput - parsed inputSpec");
        } else {
            String specContent = null;
            String sanitizedSpecificationUrl = sanitizeSpecificationUrl(inputSpecURL);
            try {
                specContent = loadSpecContent(sanitizedSpecificationUrl, authorizationValues);
            } catch (Exception e) {
                String msg = "Unable to read URL: " + sanitizedSpecificationUrl;
                LOGGER.error(msg, e);
                throw new IllegalArgumentException(msg);
            }

            if (StringUtils.isBlank(specContent)) {
                String msg = "Empty content found in URL: " + sanitizedSpecificationUrl;
                LOGGER.error(msg);
                throw new IllegalArgumentException(msg);
            }
            config.setInputSpec(specContent);

            config.setInputURL(sanitizedSpecificationUrl);
            ParseOptions options = buildParseOptions();
            SwaggerParseResult result = new OpenAPIParser().readLocation(sanitizedSpecificationUrl, authorizationValues, options);

            OpenAPI openAPI = result.getOpenAPI();
            LOGGER.debug("getClientOptInput - parsed inputSpecURL " + sanitizedSpecificationUrl);
            input.opts(new ClientOpts())
                    .openAPI(openAPI);

            if (config.needsUnflattenedSpec()) {
                ParseOptions optionsUnflattened = buildUnflattenedParseOptions();
                SwaggerParseResult resultUnflattened = new OpenAPIParser().readLocation(sanitizedSpecificationUrl, authorizationValues, optionsUnflattened);
                OpenAPI openAPIUnflattened = resultUnflattened.getOpenAPI();
                config.setUnflattenedOpenAPI(openAPIUnflattened);
            }

        }

        config.setOutputDir(outputDir);
        config.setSkipOverwrite(skipOverwrite);
        config.setIgnoreFilePathOverride(ignoreFileOverride);
        config.setRemoveOperationIdPrefix(removeOperationIdPrefix);

        config.instantiationTypes().putAll(instantiationTypes);
        config.typeMapping().putAll(typeMappings);
        config.importMapping().putAll(importMappings);
        config.languageSpecificPrimitives().addAll(languageSpecificPrimitives);
        config.reservedWordsMappings().putAll(reservedWordMappings);

        config.setLanguageArguments(codegenArguments);

        checkAndSetAdditionalProperty(apiPackage, CodegenConstants.API_PACKAGE);
        checkAndSetAdditionalProperty(modelPackage, CodegenConstants.MODEL_PACKAGE);
        checkAndSetAdditionalProperty(invokerPackage, CodegenConstants.INVOKER_PACKAGE);
        checkAndSetAdditionalProperty(groupId, CodegenConstants.GROUP_ID);
        checkAndSetAdditionalProperty(artifactId, CodegenConstants.ARTIFACT_ID);
        checkAndSetAdditionalProperty(artifactVersion, CodegenConstants.ARTIFACT_VERSION);
        checkAndSetAdditionalProperty(templateDir, toAbsolutePathStr(templateDir), CodegenConstants.TEMPLATE_DIR);
        checkAndSetAdditionalProperty(templateVersion, CodegenConstants.TEMPLATE_VERSION);
        checkAndSetAdditionalProperty(modelNamePrefix, CodegenConstants.MODEL_NAME_PREFIX);
        checkAndSetAdditionalProperty(modelNameSuffix, CodegenConstants.MODEL_NAME_SUFFIX);
        checkAndSetAdditionalProperty(gitUserId, CodegenConstants.GIT_USER_ID);
        checkAndSetAdditionalProperty(gitRepoId, CodegenConstants.GIT_REPO_ID);
        checkAndSetAdditionalProperty(gitRepoBaseURL, CodegenConstants.GIT_REPO_BASE_URL);
        checkAndSetAdditionalProperty(releaseNote, CodegenConstants.RELEASE_NOTE);
        checkAndSetAdditionalProperty(httpUserAgent, CodegenConstants.HTTP_USER_AGENT);

        handleDynamicProperties(config);

        if (isNotEmpty(library)) {
            config.setLibrary(library);
        }

        config.additionalProperties().putAll(additionalProperties);

        input.config(config);
        return input;
    }

    private ParseOptions buildParseOptions() {
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        options.setResolveFully(resolveFully);
        options.setFlatten(true);
        options.setFlattenComposedSchemas(flattenInlineSchema);
        options.setSkipMatches(this.skipInlineModelMatches);

        if (Objects.equals(System.getenv("SAFELY_RESOLVE_URL"), "true")) {
            setSafelyResolveURLParseOptions(options);
        }

        return options;
    }

    private ParseOptions buildUnflattenedParseOptions() {
        ParseOptions options = new ParseOptions();
        options.setResolve(true);

        if (Objects.equals(System.getenv("SAFELY_RESOLVE_URL"), "true")) {
            setSafelyResolveURLParseOptions(options);
        }

        return options;
    }

    private void setSafelyResolveURLParseOptions(ParseOptions options) {
        List<String> allowList = Optional.ofNullable(System.getenv("REMOTE_REF_ALLOW_LIST"))
                .map(str -> Arrays.asList(str.split(",")))
                .orElseGet(Collections::emptyList);

        List<String> blockList = Optional.ofNullable(System.getenv("REMOTE_REF_BLOCK_LIST"))
                .map(str -> Arrays.asList(str.split(",")))
                .orElseGet(Collections::emptyList);

        options.setSafelyResolveURL(true);
        options.setRemoteRefAllowList(allowList);
        options.setRemoteRefBlockList(blockList);
    }

    @JsonAnySetter
    public CodegenConfigurator addDynamicProperty(String name, Object value) {
        dynamicProperties.put(name, value);
        return this;
    }

    @JsonAnyGetter
    public Map<String, Object> getDynamicProperties() {
        return dynamicProperties;
    }

    private void handleDynamicProperties(CodegenConfig codegenConfig) {
        for (CliOption langCliOption : codegenConfig.cliOptions()) {
            String opt = langCliOption.getOpt();
            if (dynamicProperties.containsKey(opt)) {
                codegenConfig.additionalProperties().put(opt, dynamicProperties.get(opt));
            }
            else if(systemProperties.containsKey(opt)) {
                codegenConfig.additionalProperties().put(opt, systemProperties.get(opt));
            }
        }
    }

    private void setVerboseFlags() {
        if (!verbose) {
            return;
        }
        LOGGER.info("\nVERBOSE MODE: ON. Additional debug options are injected" +
                "\n - [debugSwagger] prints the swagger specification as interpreted by the codegen" +
                "\n - [debugModels] prints models passed to the template engine" +
                "\n - [debugOperations] prints operations passed to the template engine" +
                "\n - [debugSupportingFiles] prints additional data passed to the template engine");

        System.setProperty("debugSwagger", "");
        System.setProperty("debugModels", "");
        System.setProperty("debugOperations", "");
        System.setProperty("debugSupportingFiles", "");
    }

    public void setCodegenArguments(List<CodegenArgument> codegenArguments) {
        this.codegenArguments = codegenArguments;
    }

    public List<CodegenArgument> getCodegenArguments() {
        return this.codegenArguments;
    }

    private void setSystemProperties() {
        for (Map.Entry<String, String> entry : systemProperties.entrySet()) {
            System.setProperty(entry.getKey(), entry.getValue());
        }
    }

    private static String toAbsolutePathStr(String path) {
        if (isNotEmpty(path)) {
            return Paths.get(path).toAbsolutePath().toString();
        }

        return path;

    }

    private void checkAndSetAdditionalProperty(String property, String propertyKey) {
        checkAndSetAdditionalProperty(property, property, propertyKey);
    }

    private void checkAndSetAdditionalProperty(String property, String valueToSet, String propertyKey) {
        if (isNotEmpty(property)) {
            additionalProperties.put(propertyKey, valueToSet);
        }
    }

    public static CodegenConfigurator fromFile(String configFile) {

        if (isNotEmpty(configFile)) {
            try {
                return Json.mapper().readValue(new File(configFile), CodegenConfigurator.class);
            } catch (IOException e) {
                LOGGER.error("Unable to deserialize config file: " + configFile, e);
            }
        }
        return null;
    }

    public boolean isFlattenInlineSchem() {
        return flattenInlineSchema;
    }
    public void setFlattenInlineSchema(boolean flattenInlineComposedSchemas) {
        this.flattenInlineSchema = flattenInlineComposedSchemas;
    }
}
