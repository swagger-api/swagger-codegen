package io.swagger.codegen.v3.service;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.v3.ClientOptInput;
import io.swagger.codegen.v3.CodegenArgument;
import io.swagger.codegen.v3.config.CodegenConfigurator;
import io.swagger.codegen.v3.service.exception.BadRequestException;
import io.swagger.models.Swagger;
import io.swagger.models.auth.UrlMatcher;
import io.swagger.parser.SwaggerParser;
import io.swagger.parser.util.ParseOptions;
import io.swagger.v3.core.util.Json;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class GeneratorUtil {

    protected static final Logger LOGGER = LoggerFactory.getLogger(GeneratorUtil.class);

    public static io.swagger.codegen.ClientOptInput getClientOptInputV2(GenerationRequest generationRequest) {
        LOGGER.debug("getClientOptInputV2 - start");
        final Options options = generationRequest.getOptions();
        String inputSpec = null;
        if (generationRequest.getSpec() == null) {
            inputSpec = null;
        } else if (!(generationRequest.getSpec() instanceof String)) {
            inputSpec = io.swagger.util.Json.pretty(generationRequest.getSpec());
        } else {
            inputSpec = (String)generationRequest.getSpec();
        }
        String inputSpecURL = generationRequest.getSpecURL();
        String lang = generationRequest.getLang();
        validateSpec(lang, inputSpec, inputSpecURL);
        LOGGER.debug("getClientOptInputV2 - spec validated");
        io.swagger.models.auth.UrlMatcher urlMatcher = null;
        if (!generationRequest.getOptions().getAllowedAuthHosts().isEmpty() || !generationRequest.getOptions().getDeniedAuthHosts().isEmpty()) {
            urlMatcher = url -> {
                String host = url.getHost();
                // first check denies
                for (HostAccessControl check: generationRequest.getOptions().getDeniedAuthHosts()) {
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
                for (HostAccessControl check: generationRequest.getOptions().getAllowedAuthHosts()) {
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

        final List<io.swagger.models.auth.AuthorizationValue> authorizationValues = io.swagger.codegen.auth.AuthParser.parse(generationRequest.getOptions().getAuth());
        if (!authorizationValues.isEmpty() && urlMatcher != null) {
            for (io.swagger.models.auth.AuthorizationValue authVal: authorizationValues) {
                if (authVal.getUrlMatcher() == null) {
                    authVal.setUrlMatcher(urlMatcher);
                }
            }
        }
        if (generationRequest.getOptions().getAuthorizationValue() != null) {
            io.swagger.models.auth.AuthorizationValue authorizationValue = new io.swagger.models.auth.AuthorizationValue()
                    .value(generationRequest.getOptions().getAuthorizationValue().getValue())
                    .keyName(generationRequest.getOptions().getAuthorizationValue().getKeyName())
                    .type(generationRequest.getOptions().getAuthorizationValue().getType());
            UrlMatcher predicateUrlMatcher = null;
            if (generationRequest.getOptions().getAuthorizationValue().getUrlMatcher() != null) {
                predicateUrlMatcher = url -> generationRequest.getOptions().getAuthorizationValue().getUrlMatcher().test(url);
            }
            if (predicateUrlMatcher != null) {
                authorizationValue.setUrlMatcher(predicateUrlMatcher);
            } else if (urlMatcher != null) {
                authorizationValue.setUrlMatcher(urlMatcher);
            }
            authorizationValues.add(authorizationValue);
        }
        LOGGER.debug("getClientOptInputV2 - processed auth");

        CodegenConfig codegenConfig=null;
        try {
            codegenConfig = CodegenConfigLoader.forName(lang);
        } catch(RuntimeException e) {
            throw new BadRequestException("Unsupported target " + lang + " supplied");
        }
        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        if (codegenConfig.isUsingFlattenSpec() && !Boolean.FALSE.equals(generationRequest.getOptions().isUsingFlattenSpecForV2())) {
            parseOptions.setFlatten(true);
        }
        Swagger swagger;
        if (StringUtils.isBlank(inputSpec)) {
            if (inputSpecURL != null) {
                if (!authorizationValues.isEmpty()) {
                    swagger =
                            new SwaggerParser().read(inputSpecURL, authorizationValues,
                                    parseOptions);
                } else {
                    swagger = new SwaggerParser().read(inputSpecURL, null, parseOptions);
                }
            } else {
                throw new BadRequestException("No swagger specification was supplied");
            }
            LOGGER.debug("getClientOptInputV2 - parsed inputSpecURL " + inputSpecURL);
        } else {
            try {
                JsonNode node = io.swagger.util.Json.mapper().readTree(inputSpec);
                if (!authorizationValues.isEmpty()) {
                    swagger = new SwaggerParser().read(node, authorizationValues, parseOptions);
                } else {
                    swagger = new SwaggerParser().read(node, null,parseOptions);
                }
            } catch (Exception e) {
                LOGGER.error("Exception parsing input spec", e);
                throw new BadRequestException("The swagger specification supplied was not valid");
            }
            LOGGER.debug("getClientOptInputV2 - parsed inputSpec");
        }
        if (swagger == null) {
            throw new BadRequestException("The swagger specification supplied was not valid");
        }


        io.swagger.codegen.ClientOptInput clientOptInput = new io.swagger.codegen.ClientOptInput();
        ClientOpts clientOpts = new ClientOpts();

        codegenConfig.setOutputDir(generationRequest.getOptions().getOutputDir());
        codegenConfig.setInputSpec(inputSpec);
        if (isNotEmpty(options.getApiPackage())) {
            codegenConfig.additionalProperties().put(CodegenConstants.API_PACKAGE, options.getApiPackage());
        }
        if (isNotEmpty(options.getModelPackage())) {
            codegenConfig.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, options.getModelPackage());
        }
        if (isNotEmpty(options.getModelNamePrefix())) {
            codegenConfig.additionalProperties().put(CodegenConstants.MODEL_NAME_PREFIX, options.getModelNamePrefix());
        }
        if (isNotEmpty(options.getModelNameSuffix())) {
            codegenConfig.additionalProperties().put(CodegenConstants.MODEL_NAME_SUFFIX, options.getModelNameSuffix());
        }
        if (isNotEmpty(options.getInvokerPackage())) {
            codegenConfig.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, options.getInvokerPackage());
        }
        if (isNotEmpty(options.getGroupId())) {
            codegenConfig.additionalProperties().put(CodegenConstants.GROUP_ID, options.getGroupId());
        }
        if (isNotEmpty(options.getArtifactId())) {
            codegenConfig.additionalProperties().put(CodegenConstants.ARTIFACT_ID, options.getArtifactId());
        }
        if (isNotEmpty(options.getArtifactVersion())) {
            codegenConfig.additionalProperties().put(CodegenConstants.ARTIFACT_VERSION, options.getArtifactVersion());
        }
        if (isNotEmpty(options.getLibrary())) {
            codegenConfig.setLibrary(options.getLibrary());
        }
        if (isNotEmpty(options.getGitUserId())) {
            codegenConfig.additionalProperties().put(CodegenConstants.GIT_USER_ID, options.getGitUserId());
        }
        if (isNotEmpty(options.getGitRepoId())) {
            codegenConfig.additionalProperties().put(CodegenConstants.GIT_REPO_ID, options.getGitRepoId());
        }
        if (isNotEmpty(options.getGitRepoBaseURL())) {
            codegenConfig.additionalProperties().put(CodegenConstants.GIT_REPO_BASE_URL, options.getGitRepoBaseURL());
        }
        if (isNotEmpty(options.getReleaseNote())) {
            codegenConfig.additionalProperties().put(CodegenConstants.RELEASE_NOTE, options.getReleaseNote());
        }
        if (isNotEmpty(options.getHttpUserAgent())) {
            codegenConfig.additionalProperties().put(CodegenConstants.HTTP_USER_AGENT, options.getHttpUserAgent());
        }
        if (options.getRemoveOperationIdPrefix() != null) {
            codegenConfig.setRemoveOperationIdPrefix(options.getRemoveOperationIdPrefix());
        }
        if (options.getSkipOverride() != null) {
            codegenConfig.setSkipOverwrite(options.getSkipOverride());
        }

        if (options.getInstantiationTypes() != null) {
            codegenConfig.instantiationTypes().putAll(options.getInstantiationTypes());
        }
        if (options.getImportMappings() != null) {
            codegenConfig.importMapping().putAll(options.getImportMappings());
        }
        if (options.getTypeMappings() != null) {
            codegenConfig.typeMapping().putAll(options.getTypeMappings());
        }
        if (options.getLanguageSpecificPrimitives() != null) {
            codegenConfig.languageSpecificPrimitives().addAll(options.getLanguageSpecificPrimitives());
        }
        if (options.getReservedWordsMappings() != null) {
            codegenConfig.reservedWordsMappings().putAll(options.getReservedWordsMappings());
        }
        if (options.getAdditionalProperties() != null) {
            codegenConfig.additionalProperties().putAll(options.getAdditionalProperties());
        }

        clientOptInput.opts(clientOpts).swagger(swagger);
        codegenConfig.additionalProperties().put("swagger", swagger);

        clientOptInput.setConfig(codegenConfig);
        LOGGER.debug("getClientOptInputV2 - end");
        return clientOptInput;
    }

    public static void validateSpec(String lang, String inputSpec, String inputSpecURL) {
        Validate.notEmpty(lang, "language must be specified");

        if ((StringUtils.isBlank(inputSpec) || "{}".equals(inputSpec) || "{ }".equals(inputSpec)) && StringUtils.isBlank(inputSpecURL)) {
            throw new BadRequestException("input spec or URL must be specified");
        }

    }
    public static ClientOptInput getClientOptInput(GenerationRequest generationRequest) {
        LOGGER.debug("getClientOptInput - start");
        final Options options = generationRequest.getOptions();
        String inputSpec = null;
        if (generationRequest.getSpec() == null) {
            inputSpec = null;
        } else if (!(generationRequest.getSpec() instanceof String)) {
            inputSpec = Json.pretty(generationRequest.getSpec());
        } else {
            inputSpec = (String)generationRequest.getSpec();
        }
        String inputSpecURL = generationRequest.getSpecURL();
        String lang = generationRequest.getLang();
        validateSpec(lang, inputSpec, inputSpecURL);
        LOGGER.debug("getClientOptInput - validated");
        CodegenConfigurator configurator = new CodegenConfigurator();

        configurator.setOutputDir(generationRequest.getOptions().getOutputDir());
        configurator.setInputSpec(inputSpec);
        configurator.setInputSpecURL(inputSpecURL);

        configurator.setFlattenInlineSchema(generationRequest.getOptions().isFlattenInlineComposedSchemas());

        if (isNotEmpty(lang)) {
            configurator.setLang(lang);
            readCodegenArguments(configurator, options);
        }
        if (isNotEmpty(options.getAuth())) {
            configurator.setAuth(options.getAuth());
        }
        if (options.getAuthorizationValue() != null) {
            configurator.setAuthorizationValue(options.getAuthorizationValue());
        }
        if (isNotEmpty(options.getApiPackage())) {
            configurator.setApiPackage(options.getApiPackage());
        }
        if (isNotEmpty(options.getModelPackage())) {
            configurator.setModelPackage(options.getModelPackage());
        }
        if (isNotEmpty(options.getModelNamePrefix())) {
            configurator.setModelNamePrefix(options.getModelNamePrefix());
        }
        if (isNotEmpty(options.getModelNameSuffix())) {
            configurator.setModelNameSuffix(options.getModelNameSuffix());
        }
        if (isNotEmpty(options.getInvokerPackage())) {
            configurator.setInvokerPackage(options.getInvokerPackage());
        }
        if (isNotEmpty(options.getGroupId())) {
            configurator.setGroupId(options.getGroupId());
        }
        if (isNotEmpty(options.getArtifactId())) {
            configurator.setArtifactId(options.getArtifactId());
        }
        if (options.getSkipOverride() != null) {
            configurator.setSkipOverwrite(options.getSkipOverride());
        }
        if (options.getResolveFully() != null) {
            configurator.setResolveFully(options.getResolveFully());
        }
        if (isNotEmpty(options.getArtifactVersion())) {
            configurator.setArtifactVersion(options.getArtifactVersion());
        }
        if (isNotEmpty(options.getLibrary())) {
            configurator.setLibrary(options.getLibrary());
        }
        if (isNotEmpty(options.getGitUserId())) {
            configurator.setGitUserId(options.getGitUserId());
        }
        if (isNotEmpty(options.getGitRepoId())) {
            configurator.setGitRepoId(options.getGitRepoId());
        }
        if (isNotEmpty(options.getGitRepoBaseURL())) {
            configurator.setGitRepoBaseURL(options.getGitRepoBaseURL());
        }
        if (isNotEmpty(options.getReleaseNote())) {
            configurator.setReleaseNote(options.getReleaseNote());
        }
        if (isNotEmpty(options.getTemplateVersion())) {
            configurator.setTemplateVersion(options.getTemplateVersion());
        }
        if (isNotEmpty(options.getHttpUserAgent())) {
            configurator.setHttpUserAgent(options.getHttpUserAgent());
        }
        if (options.getRemoveOperationIdPrefix() != null) {
            configurator.setRemoveOperationIdPrefix(options.getRemoveOperationIdPrefix());
        }
        if (options.getInstantiationTypes() != null) {
            for (Map.Entry<String, String> entry: options.getInstantiationTypes().entrySet()) {
                configurator.addInstantiationType(entry.getKey(), entry.getValue());
            }
        }
        if (options.getImportMappings() != null) {
            for (Map.Entry<String, String> entry: options.getImportMappings().entrySet()) {
                configurator.addImportMapping(entry.getKey(), entry.getValue());
            }
        }
        if (options.getTypeMappings() != null) {
            for (Map.Entry<String, String> entry: options.getTypeMappings().entrySet()) {
                configurator.addTypeMapping(entry.getKey(), entry.getValue());
            }
        }
        if (options.getAdditionalProperties() != null) {
            for (Map.Entry<String, Object> entry: options.getAdditionalProperties().entrySet()) {
                configurator.addAdditionalProperty(entry.getKey(), entry.getValue());
            }
        }
        if (options.getLanguageSpecificPrimitives() != null) {
            for (String key: options.getLanguageSpecificPrimitives()) {
                configurator.addLanguageSpecificPrimitive(key);
            }
        }
        if (options.getReservedWordsMappings() != null) {
            for (Map.Entry<String, String> entry: options.getReservedWordsMappings().entrySet()) {
                configurator.addAdditionalReservedWordMapping(entry.getKey(), entry.getValue());
            }
        }

        configurator.setAllowedAuthHosts(options.getAllowedAuthHosts());
        configurator.setDeniedAuthHosts(options.getDeniedAuthHosts());
        LOGGER.debug("getClientOptInput - end");
        return configurator.toClientOptInput();
    }

    private static void readCodegenArguments(CodegenConfigurator configurator, Options options) {
        if (options == null) {
            return;
        }
        io.swagger.codegen.v3.CodegenConfig config = io.swagger.codegen.v3.CodegenConfigLoader.forName(configurator.getLang());
        if (config == null) {
            return;
        }
        final List<CodegenArgument> arguments = config.readLanguageArguments();
        if (arguments == null || arguments.isEmpty()) {
            return;
        }
        for (CodegenArgument codegenArgument : arguments) {
            final String value = options.getCodegenArguments().get(codegenArgument.getOption().substring(2));
            if (value == null) {
                continue;
            }
            codegenArgument.setValue(value);
        }
        configurator.setCodegenArguments(arguments);
    }
}
