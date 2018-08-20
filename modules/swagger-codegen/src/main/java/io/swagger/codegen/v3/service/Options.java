package io.swagger.codegen.v3.service;

import io.swagger.v3.parser.core.models.AuthorizationValue;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class Options {

    private String auth;
    private AuthorizationValue authorizationValue = null;
    private String apiPackage;
    private String modelPackage;
    private String modelNamePrefix;
    private String modelNameSuffix;
    protected Map<String, String> systemProperties = new LinkedHashMap<>();
    private Map<String, String> instantiationTypes = new LinkedHashMap<>();
    private Map<String, String> typeMappings = new LinkedHashMap<>();
    private Map<String, Object> additionalProperties = new LinkedHashMap<>();
    private List<String> languageSpecificPrimitives = new ArrayList<>();
    private Map<String, String> importMappings = new LinkedHashMap<>();
    private String invokerPackage;
    private String groupId;
    private String artifactId;
    private String artifactVersion;
    private String library;
    private String gitUserId;
    private String gitRepoId;
    private String releaseNote;
    private String httpUserAgent;
    private Map<String, String> reservedWordsMappings = new LinkedHashMap<>();
    private String ignoreFileOverride;
    private String templateVersion;
    private Boolean removeOperationIdPrefix;
    private Boolean skipOverride;
    private String outputDir = "";

    public Options authorizationValue(AuthorizationValue authorizationValue) {
        this.authorizationValue = authorizationValue;
        return this;
    }

    public AuthorizationValue getAuthorizationValue() {
        return authorizationValue;
    }
    public void setAuthorizationValue(AuthorizationValue authorizationValue) {
        this.authorizationValue = authorizationValue;
    }

    public Options auth(AuthorizationValue authorizationValue) {
        this.authorizationValue = authorizationValue;
        return this;
    }

    public String getAuth() {
        return auth;
    }
    public void setAuth(String auth) {
        this.auth = auth;
    }

    public Options apiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
        return this;
    }

    public String getApiPackage() {
        return apiPackage;
    }
    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public Options outputDir(String outputDir) {
        this.outputDir = outputDir;
        return this;
    }

    public String getOutputDir() {
        return outputDir;
    }
    public void setOutputDir(String outputDir) {
        this.outputDir = outputDir;
    }

    public Options modelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
        return this;
    }

    public String getModelPackage() {
        return modelPackage;
    }
    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
    }

    public Options modelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
        return this;
    }

    public String getModelNamePrefix() {
        return modelNamePrefix;
    }
    public void setModelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
    }

    public Options modelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
        return this;
    }

    public String getModelNameSuffix() {
        return modelNameSuffix;
    }
    public void setModelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
    }

    public Options systemProperties(Map<String, String> systemProperties) {
        this.systemProperties = systemProperties;
        return this;
    }

    public Map<String, String> getSystemProperties() {
        return systemProperties;
    }

    public void setSystemProperties(Map<String, String> systemProperties) {
        this.systemProperties = systemProperties;
    }

    public Options addSystemProperty(String key, String value) {
        this.systemProperties.put(key, value);
        return this;
    }

    public Options instantiationTypes(Map<String, String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
        return this;
    }

    public Map<String, String> getInstantiationTypes() {
        return instantiationTypes;
    }

    public void setInstantiationTypes(Map<String, String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
    }

    public Options addInstantiationType(String key, String value) {
        this.instantiationTypes.put(key, value);
        return this;
    }

    public Options typeMappings(Map<String, String> typeMappings) {
        this.typeMappings = typeMappings;
        return this;
    }

    public Map<String, String> getTypeMappings() {
        return typeMappings;
    }

    public void setTypeMappings(Map<String, String> typeMappings) {
        this.typeMappings = typeMappings;
    }

    public Options addTypeMapping(String key, String value) {
        this.typeMappings.put(key, value);
        return this;
    }

    public Options additionalProperties(Map<String, Object> additionalProperties) {
        this.additionalProperties = additionalProperties;
        return this;
    }

    public Map<String, Object> getAdditionalProperties() {
        return additionalProperties;
    }

    public void setAdditionalProperties(Map<String, Object> additionalProperties) {
        this.additionalProperties = additionalProperties;
    }

    public Options addAdditionalProperty(String key, Object value) {
        this.additionalProperties.put(key, value);
        return this;
    }

    public Options importMappings(Map<String, String> importMappings) {
        this.importMappings = importMappings;
        return this;
    }

    public Map<String, String> getImportMappings() {
        return importMappings;
    }

    public void setImportMappings(Map<String, String> importMappings) {
        this.importMappings = importMappings;
    }

    public Options addImportMapping(String key, String value) {
        this.importMappings.put(key, value);
        return this;
    }

    public Options invokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
        return this;
    }

    public List<String> getLanguageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    public void setLanguageSpecificPrimitives(List<String> languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
    }

    public Options languageSpecificPrimitives(List<String>  languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
        return this;
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }
    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public Options groupId(String groupId) {
        this.groupId = groupId;
        return this;
    }

    public String getGroupId() {
        return groupId;
    }
    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public Options artifactId(String artifactId) {
        this.artifactId = artifactId;
        return this;
    }

    public String getArtifactId() {
        return artifactId;
    }
    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public Options artifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
        return this;
    }

    public String getArtifactVersion() {
        return artifactVersion;
    }
    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public Options library(String library) {
        this.library = library;
        return this;
    }

    /**
     * library template (sub-template)
     * @return library
     **/
    public String getLibrary() {
        return library;
    }
    public void setLibrary(String library) {
        this.library = library;
    }

    public Options gitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
        return this;
    }

    public String getGitUserId() {
        return gitUserId;
    }
    public void setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
    }

    public Options gitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
        return this;
    }

    public String getGitRepoId() {
        return gitRepoId;
    }
    public void setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
    }

    public Options releaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
        return this;
    }

    public String getReleaseNote() {
        return releaseNote;
    }
    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    public Options httpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
        return this;
    }

    public String getTemplateVersion() {
        return templateVersion;
    }

    public void setTemplateVersion(String templateVersion) {
        this.templateVersion = templateVersion;
    }

    public Options templateVersion(String templateVersion) {
        this.templateVersion = templateVersion;
        return this;
    }

    /**
     * HTTP user agent, e.g. codegen_csharp_api_client, default to ';Swagger-Codegen/{packageVersion}}/{language}';
     **/
    public String getHttpUserAgent() {
        return httpUserAgent;
    }
    public void setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
    }

    public Options reservedWordsMappings(Map<String, String> reservedWordsMappings) {
        this.reservedWordsMappings = reservedWordsMappings;
        return this;
    }

    public  Map<String, String> getReservedWordsMappings() {
        return reservedWordsMappings;
    }

    public void setReservedWordsMappings(Map<String, String> reservedWordsMappings) {
        this.reservedWordsMappings = reservedWordsMappings;
    }

    public Options addAdditionalReservedWordMapping(String key, String value) {
        this.reservedWordsMappings.put(key, value);
        return this;
    }

    public Options ignoreFileOverride(String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
        return this;
    }

    public String getIgnoreFileOverride() {
        return ignoreFileOverride;
    }
    public void setIgnoreFileOverride(String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
    }

    public Options removeOperationIdPrefix(Boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
        return this;
    }

    public Boolean getRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    public void setRemoveOperationIdPrefix(Boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
    }

    public Options skipOverride(Boolean skipOverride) {
        this.skipOverride = skipOverride;
        return this;
    }

    public Boolean getSkipOverride() {
        return skipOverride;
    }

    public void setSkipOverride(Boolean skipOverride) {
        this.skipOverride = skipOverride;
    }
}
