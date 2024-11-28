package io.swagger.codegen.v3;

import io.swagger.codegen.v3.ignore.CodegenIgnoreProcessor;
import io.swagger.codegen.v3.templates.TemplateEngine;
import io.swagger.codegen.v3.utils.ImplementationVersion;
import io.swagger.codegen.v3.utils.URLPathUtil;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.tags.Tag;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

//import io.swagger.codegen.languages.AbstractJavaCodegen;

public class DefaultGenerator extends AbstractGenerator implements Generator {
    protected final Logger LOGGER = LoggerFactory.getLogger(DefaultGenerator.class);
    protected CodegenConfig config;
    protected ClientOptInput opts;
    protected OpenAPI openAPI;
    protected CodegenIgnoreProcessor ignoreProcessor;
    protected TemplateEngine templateEngine;
    private Boolean generateApis = null;
    private Boolean generateModels = null;
    private Boolean generateSupportingFiles = null;
    private Boolean generateApiTests = null;
    private Boolean generateApiDocumentation = null;
    private Boolean generateModelTests = null;
    private Boolean generateModelDocumentation = null;
    private Boolean generateSwaggerMetadata = true;
    private Boolean useOas2 = false;
    private String basePath;
    private String basePathWithoutHost;
    private String contextPath;
    private Map<String, String> generatorPropertyDefaults = new HashMap<>();

    @Override
    public Generator opts(ClientOptInput opts) {
        this.opts = opts;
        this.openAPI = opts.getOpenAPI();
        this.config = opts.getConfig();
        this.config.additionalProperties().putAll(opts.getOpts().getProperties());

        String ignoreFileLocation = this.config.getIgnoreFilePathOverride();
        if(ignoreFileLocation != null) {
            final File ignoreFile = new File(ignoreFileLocation);
            if(ignoreFile.exists() && ignoreFile.canRead()) {
                this.ignoreProcessor = new CodegenIgnoreProcessor(ignoreFile);
            } else {
                LOGGER.warn("Ignore file specified at {} is not valid. This will fall back to an existing ignore file if present in the output directory.", ignoreFileLocation);
            }
        }

        if(this.ignoreProcessor == null) {
            this.ignoreProcessor = new CodegenIgnoreProcessor(this.config.getOutputDir());
        }
        return this;
    }

    /**
     * Programmatically disable the output of .swagger-codegen/VERSION, .swagger-codegen-ignore,
     * or other metadata files used by Swagger Codegen.
     * @param generateSwaggerMetadata true: enable outputs, false: disable outputs
     */
    @SuppressWarnings("WeakerAccess")
    public void setGenerateSwaggerMetadata(Boolean generateSwaggerMetadata) {
        this.generateSwaggerMetadata = generateSwaggerMetadata;
    }

    /**
     * Set generator properties otherwise pulled from system properties.
     * Useful for running tests in parallel without relying on System.properties.
     * @param key The system property key
     * @param value The system property value
     */
    @SuppressWarnings("WeakerAccess")
    public void setGeneratorPropertyDefault(final String key, final String value) {
        this.generatorPropertyDefaults.put(key, value);
    }

    private Boolean getGeneratorPropertyDefaultSwitch(final String key, final Boolean defaultValue) {
        String result = null;
        if (this.generatorPropertyDefaults.containsKey(key)) {
            result = this.generatorPropertyDefaults.get(key);
        }
        if (result != null) {
            return Boolean.valueOf(result);
        }
        return defaultValue;
    }

    private String getScheme() {
        String scheme = URLPathUtil.getScheme(this.openAPI, this.config);
        if (StringUtils.isBlank(scheme)) {
            scheme = "https";
        }
        scheme = config.escapeText(scheme);
        return scheme;
    }

    private void configureGeneratorProperties() {
        // allows generating only models by specifying a CSV of models to generate, or empty for all
        // NOTE: Boolean.TRUE is required below rather than `true` because of JVM boxing constraints and type inference.

        if (System.getProperty(CodegenConstants.GENERATE_APIS) != null) {
            generateApis = Boolean.valueOf(System.getProperty(CodegenConstants.GENERATE_APIS));
        } else {
            generateApis = System.getProperty(CodegenConstants.APIS) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.APIS, null);
        }
        if (System.getProperty(CodegenConstants.GENERATE_MODELS) != null) {
            generateModels = Boolean.valueOf(System.getProperty(CodegenConstants.GENERATE_MODELS));
        } else {
            generateModels = System.getProperty(CodegenConstants.MODELS) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODELS, null);
        }
        String supportingFilesProperty = System.getProperty(CodegenConstants.SUPPORTING_FILES);
        if (((supportingFilesProperty != null) && supportingFilesProperty.equalsIgnoreCase("false"))) {
            generateSupportingFiles = false;
        } else {
            generateSupportingFiles = supportingFilesProperty != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.SUPPORTING_FILES, null);
        }

        if (generateApis == null && generateModels == null && generateSupportingFiles == null) {
            // no specifics are set, generate everything
            generateApis = generateModels = generateSupportingFiles = true;
        } else {
            if(generateApis == null) {
                generateApis = false;
            }
            if(generateModels == null) {
                generateModels = false;
            }
            if(generateSupportingFiles == null) {
                generateSupportingFiles = false;
            }
        }
        // model/api tests and documentation options rely on parent generate options (api or model) and no other options.
        // They default to true in all scenarios and can only be marked false explicitly
        Boolean generateModelTestsOption = getCustomOptionBooleanValue(CodegenConstants.MODEL_TESTS_OPTION);
        if (generateModelTestsOption == null) {
            generateModelTestsOption = System.getProperty(CodegenConstants.MODEL_TESTS) != null ? Boolean.valueOf(System.getProperty(CodegenConstants.MODEL_TESTS)) : null;
        }
        Boolean generateModelDocsOption = getCustomOptionBooleanValue(CodegenConstants.MODEL_DOCS_OPTION);
        if (generateModelDocsOption == null) {
            generateModelDocsOption = System.getProperty(CodegenConstants.MODEL_DOCS) != null ? Boolean.valueOf(System.getProperty(CodegenConstants.MODEL_DOCS)) : null;
        }
        Boolean generateAPITestsOption = getCustomOptionBooleanValue(CodegenConstants.API_TESTS_OPTION);
        if (generateAPITestsOption == null) {
            generateAPITestsOption = System.getProperty(CodegenConstants.API_TESTS) != null ? Boolean.valueOf(System.getProperty(CodegenConstants.API_TESTS)) : null;
        }
        Boolean generateAPIDocsOption = getCustomOptionBooleanValue(CodegenConstants.API_DOCS_OPTION);
        if (generateAPIDocsOption == null) {
            generateAPIDocsOption = System.getProperty(CodegenConstants.API_DOCS) != null ? Boolean.valueOf(System.getProperty(CodegenConstants.API_DOCS)) : null;
        }
        Boolean useOas2Option = getCustomOptionBooleanValue(CodegenConstants.USE_OAS2_OPTION);

        generateModelTests = generateModelTestsOption != null ? generateModelTestsOption : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODEL_TESTS, true);
        generateModelDocumentation = generateModelDocsOption != null ? generateModelDocsOption : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODEL_DOCS, true);
        generateApiTests = generateAPITestsOption != null ? generateAPITestsOption : getGeneratorPropertyDefaultSwitch(CodegenConstants.API_TESTS, true);
        generateApiDocumentation = generateAPIDocsOption != null ? generateAPIDocsOption : getGeneratorPropertyDefaultSwitch(CodegenConstants.API_DOCS, true);
        useOas2 = useOas2Option != null ? useOas2Option : getGeneratorPropertyDefaultSwitch(CodegenConstants.USE_OAS2, false);

        // Additional properties added for tests to exclude references in project related files
        config.additionalProperties().put(CodegenConstants.GENERATE_API_TESTS, generateApiTests);
        config.additionalProperties().put(CodegenConstants.GENERATE_MODEL_TESTS, generateModelTests);

        config.additionalProperties().put(CodegenConstants.GENERATE_API_DOCS, generateApiDocumentation);
        config.additionalProperties().put(CodegenConstants.GENERATE_MODEL_DOCS, generateModelDocumentation);

        // Additional properties could be set already (f.e. using Maven plugin)
        if (useOas2Option != null && !config.additionalProperties().containsKey(CodegenConstants.USE_OAS2)) {
            config.additionalProperties().put(CodegenConstants.USE_OAS2, useOas2);
        }

        if(!generateApiTests && !generateModelTests) {
            config.additionalProperties().put(CodegenConstants.EXCLUDE_TESTS, true);
        }
        if (System.getProperty("debugSwagger") != null) {
            Json.prettyPrint(this.openAPI);
        }
        config.processOpts();
        config.preprocessOpenAPI(this.openAPI);
        config.additionalProperties().put("generatorVersion", ImplementationVersion.read());
        config.additionalProperties().put("generatedDate", ZonedDateTime.now().toString());
        config.additionalProperties().put("generatedYear", String.valueOf(ZonedDateTime.now().getYear()));
        config.additionalProperties().put("generatorClass", config.getClass().getName());
        config.additionalProperties().put("inputSpec", config.getInputSpec());
        if (this.openAPI.getExtensions() != null) {
            config.vendorExtensions().putAll(this.openAPI.getExtensions());
        }

        this.templateEngine = config.getTemplateEngine();

        URL url = URLPathUtil.getServerURL(openAPI, config);

        contextPath = config.escapeText(url == null ? StringUtils.EMPTY : url.getPath());
        basePath = config.escapeText(URLPathUtil.getHost(openAPI));
        basePathWithoutHost = config.escapeText(contextPath);

    }

    private void configureSwaggerInfo() {
        Info info = openAPI.getInfo();
        if (info == null) {
            return;
        }
        if (info.getTitle() != null) {
            config.additionalProperties().put("appName", config.escapeText(info.getTitle()));
        }
        if (info.getVersion() != null) {
            config.additionalProperties().put("appVersion", config.escapeText(info.getVersion()));
        } else {
            LOGGER.error("Missing required field info version. Default appVersion set to 1.0.0");
            config.additionalProperties().put("appVersion", "1.0.0");
        }

        if (StringUtils.isEmpty(info.getDescription())) {
            // set a default description if none is provided
            config.additionalProperties().put("appDescription",
                    "No description provided (generated by Swagger Codegen https://github.com/swagger-api/swagger-codegen)");
            config.additionalProperties().put("unescapedAppDescription", "No description provided (generated by Swagger Codegen https://github.com/swagger-api/swagger-codegen)");
        } else {
            config.additionalProperties().put("appDescription", config.escapeText(info.getDescription()));
            config.additionalProperties().put("unescapedAppDescription", info.getDescription());
        }

        if (info.getContact() != null) {
            Contact contact = info.getContact();
            if (contact.getEmail() != null) {
                config.additionalProperties().put("infoEmail", config.escapeText(contact.getEmail()));
            }
            if (contact.getName() != null) {
                config.additionalProperties().put("infoName", config.escapeText(contact.getName()));
            }
            if (contact.getUrl() != null) {
                config.additionalProperties().put("infoUrl", config.escapeText(contact.getUrl()));
            }
        }

        if (info.getLicense() != null) {
            License license = info.getLicense();
            if (license.getName() != null) {
                config.additionalProperties().put("licenseInfo", config.escapeText(license.getName()));
            }
            if (license.getUrl() != null) {
                config.additionalProperties().put("licenseUrl", config.escapeText(license.getUrl()));
            }
        }

        if (info.getVersion() != null) {
            config.additionalProperties().put("version", config.escapeText(info.getVersion()));
        } else {
            LOGGER.error("Missing required field info version. Default version set to 1.0.0");
            config.additionalProperties().put("version", "1.0.0");
        }

        if (info.getTermsOfService() != null) {
            config.additionalProperties().put("termsOfService", config.escapeText(info.getTermsOfService()));
        }
    }

    private void generateModelTests(List<File> files, Map<String, Object> models, String modelName) throws IOException{
        // to generate model test files
        for (String templateName : config.modelTestTemplateFiles().keySet()) {
            String suffix = config.modelTestTemplateFiles().get(templateName);
            String filename = config.modelTestFileFolder() + File.separator + config.toModelTestFilename(modelName) + suffix;
            // do not overwrite test file that already exists
            if (new File(filename).exists()) {
                LOGGER.info("File exists. Skipped overwriting " + filename);
                continue;
            }
            File written = processTemplateToFile(models, templateName, filename);
            if (written != null) {
                files.add(written);
            }
        }
    }

    private void generateModelDocumentation(List<File> files, Map<String, Object> models, String modelName) throws IOException {
        for (String templateName : config.modelDocTemplateFiles().keySet()) {
            String suffix = config.modelDocTemplateFiles().get(templateName);
            String filename = config.modelDocFileFolder() + File.separator + config.toModelDocFilename(modelName) + suffix;
            if (!config.shouldOverwrite(filename)) {
                LOGGER.info("Skipped overwriting " + filename);
                continue;
            }
            File written = processTemplateToFile(models, templateName, filename);
            if (written != null) {
                files.add(written);
            }
        }
    }

    private void generateModels(List<File> files, List<Object> allModels) {

        if (!generateModels) {
            return;
        }

        final Map<String, Schema> schemas = this.openAPI.getComponents().getSchemas();
        if (schemas == null) {
            return;
        }

        String modelNames = System.getProperty("models");
        Set<String> modelsToGenerate = null;
        if(modelNames != null && !modelNames.isEmpty()) {
            modelsToGenerate = new HashSet<>(Arrays.asList(modelNames.split(",")));
        }

        Set<String> modelKeys = schemas.keySet();
        if(modelsToGenerate != null && !modelsToGenerate.isEmpty()) {
            Set<String> updatedKeys = new HashSet<>();
            for(String m : modelKeys) {
                if(modelsToGenerate.contains(m)) {
                    updatedKeys.add(m);
                }
            }
            modelKeys = updatedKeys;
        }

        // store all processed models
        Map<String,Object> allProcessedModels = new TreeMap<>(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return ObjectUtils.compare(config.toModelName(o1), config.toModelName(o2));
            }
        });

        // process models only
        for (String name : modelKeys) {
            try {
                //don't generate models that have an import mapping
                if(!config.getIgnoreImportMapping() && config.importMapping().containsKey(name)) {
                    LOGGER.info("Model " + name + " not imported due to import mapping");
                    continue;
                }
                Schema schema = schemas.get(name);
                Map<String, Schema> schemaMap = new HashMap<>();
                schemaMap.put(name, schema);
                Map<String, Object> models = processModels(config, schemaMap, schemas);
                models.put("classname", config.toModelName(name));
                models.putAll(config.additionalProperties());
                allProcessedModels.put(name, models);

                final List<Object> modelList  = (List<Object>) models.get("models");

                if (modelList == null || modelList.isEmpty()) {
                    continue;
                }
            } catch (Exception e) {
                throw new RuntimeException("Could not process model '" + name + "'" + ".Please make sure that your schema is correct!", e);
            }
        }

        final ISchemaHandler schemaHandler = config.getSchemaHandler();
        schemaHandler.readProcessedModels(allProcessedModels);

        final List<CodegenModel> composedModels = schemaHandler.getModels();

         if (composedModels != null && !composedModels.isEmpty()) {
            for (CodegenModel composedModel : composedModels) {
                if (allProcessedModels.get(composedModel.name) != null) {
                    final Map<String, Object> models = (Map<String, Object>) allProcessedModels.get(composedModel.name);
                    models.put("x-is-composed-model", composedModel.isComposedModel);
                    continue;
                }
                final Map<String, Object> models = processModel(composedModel, config, schemas);
                models.put("classname", config.toModelName(composedModel.name));
                models.put("x-is-composed-model", composedModel.isComposedModel);
                models.putAll(config.additionalProperties());
                allProcessedModels.put(composedModel.name, models);
            }
        }

        // post process all processed models
        allProcessedModels = config.postProcessAllModels(allProcessedModels);

        // generate files based on processed models
        for (String modelName: allProcessedModels.keySet()) {
            Map<String, Object> models = (Map<String, Object>)allProcessedModels.get(modelName);
            try {
                //don't generate models that have an import mapping
                if(!config.getIgnoreImportMapping() && config.importMapping().containsKey(modelName)) {
                    continue;
                }
                Map<String, Object> modelTemplate = (Map<String, Object>) ((List<Object>) models.get("models")).get(0);
                if (config.checkAliasModel()) {
                    // Special handling of aliases only applies to Java
                    if (modelTemplate != null && modelTemplate.containsKey("model")) {
                        CodegenModel codegenModel = (CodegenModel) modelTemplate.get("model");
                        Map<String, Object> vendorExtensions = codegenModel.getVendorExtensions();
                        boolean isAlias = false;
                        if (vendorExtensions.get(CodegenConstants.IS_ALIAS_EXT_NAME) != null) {
                            isAlias = Boolean.parseBoolean(vendorExtensions.get(CodegenConstants.IS_ALIAS_EXT_NAME).toString());
                        }
                        if (isAlias) {
                            continue;  // Don't create user-defined classes for aliases
                        }
                    }
                }
                allModels.add(modelTemplate);
                for (String templateName : config.modelTemplateFiles().keySet()) {
                    String suffix = config.modelTemplateFiles().get(templateName);
                    String filename = config.modelFileFolder() + File.separator + config.toModelFilename(modelName) + suffix;
                    if (!config.shouldOverwrite(filename)) {
                        LOGGER.info("Skipped overwriting " + filename);
                        continue;
                    }
                    File written = processTemplateToFile(models, templateName, filename);
                    if(written != null) {
                        files.add(written);
                    }
                }
                if(generateModelTests) {
                    generateModelTests(files, models, modelName);
                }
                if(generateModelDocumentation) {
                    // to generate model documentation files
                    generateModelDocumentation(files, models, modelName);
                }
            } catch (Exception e) {
                throw new RuntimeException("Could not generate model '" + modelName + "'", e);
            }
        }
        if (System.getProperty("debugModels") != null) {
            LOGGER.info("############ Model info ############");
            Json.prettyPrint(allModels);
        }

    }

    private void generateApis(List<File> files, List<Object> allOperations, List<Object> allModels) {
        if (!generateApis) {
            return;
        }
        boolean hasModel = true;
        if (allModels == null || allModels.isEmpty()) {
            hasModel = false;
        }

        if (this.openAPI.getPaths() == null) {
            return;
        }
        Map<String, List<CodegenOperation>> paths = processPaths(this.openAPI.getPaths());
        Set<String> apisToGenerate = null;
        String apiNames = System.getProperty("apis");
        if(apiNames != null && !apiNames.isEmpty()) {
            apisToGenerate = new HashSet<String>(Arrays.asList(apiNames.split(",")));
        }
        if(apisToGenerate != null && !apisToGenerate.isEmpty()) {
            Map<String, List<CodegenOperation>> updatedPaths = new TreeMap<>();
            for(String m : paths.keySet()) {
                if(apisToGenerate.contains(m)) {
                    updatedPaths.put(m, paths.get(m));
                }
            }
            paths = updatedPaths;
        }
        for (String tag : paths.keySet()) {
            try {
                List<CodegenOperation> ops = paths.get(tag);
                Collections.sort(ops, new Comparator<CodegenOperation>() {
                    @Override
                    public int compare(CodegenOperation one, CodegenOperation another) {
                        return ObjectUtils.compare(one.operationId, another.operationId);
                    }
                });
                Map<String, Object> operation = processOperations(config, tag, ops, allModels);

                processSecurityProperties(operation);

                operation.put("basePath", basePath);
                operation.put("basePathWithoutHost", basePathWithoutHost);
                operation.put("contextPath", contextPath);
                operation.put("baseName", tag);
                operation.put("modelPackage", config.modelPackage());
                operation.putAll(config.additionalProperties());
                operation.put("classname", config.toApiName(tag));
                operation.put("classVarName", config.toApiVarName(tag));
                operation.put("importPath", config.toApiImport(tag));
                operation.put("classFilename", config.toApiFilename(tag));

                if(!config.vendorExtensions().isEmpty()) {
                    operation.put("vendorExtensions", config.vendorExtensions());
                }

                // Pass sortParamsByRequiredFlag through to the Mustache template...
                boolean sortParamsByRequiredFlag = true;
                if (this.config.additionalProperties().containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
                    sortParamsByRequiredFlag = Boolean.valueOf(this.config.additionalProperties().get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString());
                }
                operation.put("sortParamsByRequiredFlag", sortParamsByRequiredFlag);

                operation.put("hasModel", hasModel);


                allOperations.add(new HashMap<>(operation));
                for (int i = 0; i < allOperations.size(); i++) {
                    Map<String, Object> oo = (Map<String, Object>) allOperations.get(i);
                    if (i < (allOperations.size() - 1)) {
                        oo.put("hasMore", "true");
                    }
                }

                for (String templateName : config.apiTemplateFiles().keySet()) {
                    String filename = config.apiFilename(templateName, tag);
                    if (!config.shouldOverwrite(filename) && new File(filename).exists()) {
                        LOGGER.info("Skipped overwriting " + filename);
                        continue;
                    }

                    File written = processTemplateToFile(operation, templateName, filename);
                    if(written != null) {
                        files.add(written);
                    }
                }

                if(generateApiTests) {
                    // to generate api test files
                    for (String templateName : config.apiTestTemplateFiles().keySet()) {
                        String filename = config.apiTestFilename(templateName, tag);
                        // do not overwrite test file that already exists
                        if (new File(filename).exists()) {
                            LOGGER.info("File exists. Skipped overwriting " + filename);
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if (written != null) {
                            files.add(written);
                        }
                    }
                }


                if(generateApiDocumentation) {
                    // to generate api documentation files
                    for (String templateName : config.apiDocTemplateFiles().keySet()) {
                        String filename = config.apiDocFilename(templateName, tag);
                        if (!config.shouldOverwrite(filename) && new File(filename).exists()) {
                            LOGGER.info("Skipped overwriting " + filename);
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if (written != null) {
                            files.add(written);
                        }
                    }
                }

            } catch (Exception e) {
                throw new RuntimeException("Could not generate api file for '" + tag + "'", e);
            }
        }
        if (System.getProperty("debugOperations") != null) {
            LOGGER.info("############ Operation info ############");
            Json.prettyPrint(allOperations);
        }

    }

    private void generateSupportingFiles(List<File> files, Map<String, Object> bundle) {
        if (!generateSupportingFiles) {
            return;
        }
        Set<String> supportingFilesToGenerate = null;
        String supportingFiles = System.getProperty(CodegenConstants.SUPPORTING_FILES);
        boolean generateAll = false;
        if (supportingFiles != null && supportingFiles.equalsIgnoreCase("true")) {
            generateAll = true;
        } else if (supportingFiles != null && !supportingFiles.isEmpty()) {
            supportingFilesToGenerate = new HashSet<>(Arrays.asList(supportingFiles.split(",")));
        }

        for (SupportingFile support : config.supportingFiles()) {
            try {
                String outputFolder = config.outputFolder();
                if (StringUtils.isNotEmpty(support.folder)) {
                    outputFolder += File.separator + support.folder;
                }
                File of = new File(outputFolder);
                if (!of.isDirectory()) {
                    of.mkdirs();
                }
                String outputFilename = outputFolder + File.separator + support.destinationFilename.replace('/', File.separatorChar);
                if (!config.shouldOverwrite(outputFilename)) {
                    LOGGER.info("Skipped overwriting " + outputFilename);
                    continue;
                }
                String templateFile;
                if( support instanceof GlobalSupportingFile) {
                    templateFile = config.getCommonTemplateDir() + File.separator +  support.templateFile;
                } else {
                    templateFile = getFullTemplateFile(config, support.templateFile);
                }

                boolean shouldGenerate = true;
                if(!generateAll && supportingFilesToGenerate != null && !supportingFilesToGenerate.isEmpty()) {
                    shouldGenerate = supportingFilesToGenerate.contains(support.destinationFilename);
                }
                if (!shouldGenerate){
                    continue;
                }

                if(ignoreProcessor.allowsFile(new File(outputFilename))) {
                    if (templateFile.endsWith("mustache")) {
                        String rendered = templateEngine.getRendered(templateFile, bundle);
                        writeToFile(outputFilename, rendered);
                        files.add(new File(outputFilename));
                    } else {
                        InputStream in = null;

                        try {
                            in = new FileInputStream(templateFile);
                        } catch (Exception e) {
                            // continue
                        }
                        if (in == null) {
                            in = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(templateFile));
                        }
                        File outputFile = new File(outputFilename);
                        OutputStream out = new FileOutputStream(outputFile, false);
                        if (in != null) {
                            LOGGER.info("writing file " + outputFile);
                            IOUtils.copy(in, out);
                            out.close();
                        } else {
                            LOGGER.warn("can't open " + templateFile + " for input");
                        }
                        files.add(outputFile);
                    }
                } else {
                    LOGGER.info("Skipped generation of " + outputFilename + " due to rule in .swagger-codegen-ignore");
                }
            } catch (Exception e) {
                throw new RuntimeException("Could not generate supporting file '" + support + "'", e);
            }
        }

        // Consider .swagger-codegen-ignore a supporting file
        // Output .swagger-codegen-ignore if it doesn't exist and wasn't explicitly created by a generator
        final String swaggerCodegenIgnore = ".swagger-codegen-ignore";
        String ignoreFileNameTarget = config.outputFolder() + File.separator + swaggerCodegenIgnore;
        File ignoreFile = new File(ignoreFileNameTarget);
        if (generateSwaggerMetadata && !ignoreFile.exists()) {
            String ignoreFileNameSource = File.separator + config.getCommonTemplateDir() + File.separator + swaggerCodegenIgnore;
            String ignoreFileContents = readResourceContents(ignoreFileNameSource);
            try {
                writeToFile(ignoreFileNameTarget, ignoreFileContents);
            } catch (IOException e) {
                throw new RuntimeException("Could not generate supporting file '" + swaggerCodegenIgnore + "'", e);
            }
            files.add(ignoreFile);
        }

        if(generateSwaggerMetadata) {
            final String swaggerVersionMetadata = config.outputFolder() + File.separator + ".swagger-codegen" + File.separator + "VERSION";
            File swaggerVersionMetadataFile = new File(swaggerVersionMetadata);
            try {
                writeToFile(swaggerVersionMetadata, ImplementationVersion.read());
                files.add(swaggerVersionMetadataFile);
            } catch (IOException e) {
                throw new RuntimeException("Could not generate supporting file '" + swaggerVersionMetadata + "'", e);
            }
        }

        /*
         * The following code adds default LICENSE (Apache-2.0) for all generators
         * To use license other than Apache2.0, update the following file:
         *   modules/swagger-codegen/src/main/resources/_common/LICENSE
         *
        final String apache2License = "LICENSE";
        String licenseFileNameTarget = config.outputFolder() + File.separator + apache2License;
        File licenseFile = new File(licenseFileNameTarget);
        String licenseFileNameSource = File.separator + config.getCommonTemplateDir() + File.separator + apache2License;
        String licenseFileContents = readResourceContents(licenseFileNameSource);
        try {
            writeToFile(licenseFileNameTarget, licenseFileContents);
        } catch (IOException e) {
            throw new RuntimeException("Could not generate LICENSE file '" + apache2License + "'", e);
        }
        files.add(licenseFile);
         */

    }

    private Map<String, Object> buildSupportFileBundle(List<Object> allOperations, List<Object> allModels) {

        Map<String, Object> bundle = new HashMap<>();
        bundle.putAll(config.additionalProperties());
        bundle.put("apiPackage", config.apiPackage());

        Map<String, Object> apis = new HashMap<>();
        apis.put("apis", allOperations);

        URL url = URLPathUtil.getServerURL(openAPI, config);

        if (url != null) {
            bundle.put("host", url.getHost());
        }

        bundle.put("openAPI", openAPI);
        bundle.put("basePath", basePath);
        bundle.put("basePathWithoutHost",basePathWithoutHost);
        bundle.put("scheme", URLPathUtil.getScheme(openAPI, config));
        bundle.put("contextPath", contextPath);
        bundle.put("apiInfo", apis);
        bundle.put("models", allModels);
        boolean hasModel = true;
        if (allModels == null || allModels.isEmpty()) {
            hasModel = false;
        }
        bundle.put("hasModel", hasModel);
        bundle.put("apiFolder", config.apiPackage().replace('.', File.separatorChar));
        bundle.put("modelPackage", config.modelPackage());

        processSecurityProperties(bundle);

        if (openAPI.getExternalDocs() != null) {
            bundle.put("externalDocs", openAPI.getExternalDocs());
        }
        for (int i = 0; i < allModels.size() - 1; i++) {
            HashMap<String, CodegenModel> cm = (HashMap<String, CodegenModel>) allModels.get(i);
            CodegenModel m = cm.get("model");
            m.getVendorExtensions().put(CodegenConstants.HAS_MORE_MODELS_EXT_NAME, Boolean.TRUE);
        }

        config.postProcessSupportingFileData(bundle);

        if (System.getProperty("debugSupportingFiles") != null) {
            LOGGER.info("############ Supporting file info ############");
            Json.prettyPrint(bundle);
        }
        return bundle;
    }

    @Override
    public List<File> generate() {

        if (openAPI == null) {
            throw new RuntimeException("missing OpenAPI input!");
        }
        if (config == null) {
            throw new RuntimeException("missing configuration input!");
        }
        configureGeneratorProperties();
        configureSwaggerInfo();

        List<File> files = new ArrayList<>();
        // models
        List<Object> allModels = new ArrayList<>();
        generateModels(files, allModels);
        // apis
        List<Object> allOperations = new ArrayList<>();
        generateApis(files, allOperations, allModels);

        // supporting files
        Map<String, Object> bundle = buildSupportFileBundle(allOperations, allModels);
        generateSupportingFiles(files, bundle);
        config.processOpenAPI(openAPI);
        return files;
    }

    private File processTemplateToFile(Map<String, Object> templateData, String templateName, String outputFilename) throws IOException {
        String adjustedOutputFilename = outputFilename.replaceAll("//", "/").replace('/', File.separatorChar);
        if(ignoreProcessor.allowsFile(new File(adjustedOutputFilename))) {
            String templateFile = getFullTemplateFile(config, templateName);
            String rendered = templateEngine.getRendered(templateFile, templateData);
            writeToFile(adjustedOutputFilename, rendered);
            return new File(adjustedOutputFilename);
        }

        LOGGER.info("Skipped generation of " + adjustedOutputFilename + " due to rule in .swagger-codegen-ignore");
        return null;
    }

    private static void processMimeTypes(List<String> mimeTypeList, Map<String, Object> operation, String source) {
        if (mimeTypeList == null || mimeTypeList.isEmpty()){
            return;
        }
        List<Map<String, String>> c = new ArrayList<>();
        int count = 0;
        for (String key : mimeTypeList) {
            Map<String, String> mediaType = new HashMap<>();
            mediaType.put("mediaType", key);
            count += 1;
            if (count < mimeTypeList.size()) {
                mediaType.put("hasMore", "true");
            } else {
                mediaType.put("hasMore", null);
            }
            c.add(mediaType);
        }
        operation.put(source, c);
        String flagFieldName = "has" + source.substring(0, 1).toUpperCase() + source.substring(1);
        operation.put(flagFieldName, true);

    }

    public Map<String, List<CodegenOperation>> processPaths(Paths paths) {
        Map<String, List<CodegenOperation>> ops = new TreeMap<>();
        for (String resourcePath : paths.keySet()) {
            PathItem path = paths.get(resourcePath);
            processOperation(resourcePath, "get", path.getGet(), ops, path);
            processOperation(resourcePath, "head", path.getHead(), ops, path);
            processOperation(resourcePath, "put", path.getPut(), ops, path);
            processOperation(resourcePath, "post", path.getPost(), ops, path);
            processOperation(resourcePath, "delete", path.getDelete(), ops, path);
            processOperation(resourcePath, "patch", path.getPatch(), ops, path);
            processOperation(resourcePath, "options", path.getOptions(), ops, path);
        }
        return ops;
    }

    private void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations, PathItem path) {
        if (operation == null) {
            return;
        }
        if (System.getProperty("debugOperations") != null) {
            LOGGER.info("processOperation: resourcePath= " + resourcePath + "\t;" + httpMethod + " " + operation + "\n");
        }
        List<Tag> tags = new ArrayList<>();

        List<String> tagNames = operation.getTags();
        List<Tag> swaggerTags = this.openAPI.getTags();
        if (tagNames != null) {
            if (swaggerTags == null) {
                for (String tagName : tagNames) {
                    tags.add(new Tag().name(tagName));
                }
            } else {
                for (String tagName : tagNames) {
                    boolean foundTag = false;
                    for (Tag tag : swaggerTags) {
                        if (tag.getName().equals(tagName)) {
                            tags.add(tag);
                            foundTag = true;
                            break;
                        }
                    }

                    if (!foundTag) {
                        tags.add(new Tag().name(tagName));
                    }
                }
            }
        }

        if (tags.isEmpty()) {
            tags.add(new Tag().name("default"));
        }

        /*
         build up a set of parameter "ids" defined at the operation level
         per the swagger 2.0 spec "A unique parameter is defined by a combination of a name and location"
          i'm assuming "location" == "in"
        */
        Set<String> operationParameters = new HashSet<>();
        if (operation.getParameters() != null) {
            for (Parameter parameter : operation.getParameters()) {
                operationParameters.add(generateParameterId(parameter));
            }
        }

        //need to propagate path level down to the operation
        if (path.getParameters() != null) {
            for (Parameter parameter : path.getParameters()) {
                //skip propagation if a parameter with the same name is already defined at the operation level
                if (!operationParameters.contains(generateParameterId(parameter)) && operation.getParameters() != null) {
                    operation.getParameters().add(parameter);
                }
            }
        }

        final Map<String, Schema> schemas = openAPI.getComponents() != null ? openAPI.getComponents().getSchemas() : null;
        final Map<String, SecurityScheme> securitySchemes = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
        final List<SecurityRequirement> globalSecurities = openAPI.getSecurity();
        for (Tag tag : tags) {
            try {
                CodegenOperation codegenOperation = config.fromOperation(resourcePath, httpMethod, operation, schemas, openAPI);
                codegenOperation.tags = new ArrayList<>(tags);
                config.addOperationToGroup(config.sanitizeTag(tag.getName()), resourcePath, operation, codegenOperation, operations);

                List<SecurityRequirement> securities = operation.getSecurity();
                if (securities != null && securities.isEmpty()) {
                    continue;
                }
                Map<String, SecurityScheme> authMethods = getAuthMethods(securities, securitySchemes);
                if (authMethods == null || authMethods.isEmpty()) {
                    authMethods = getAuthMethods(globalSecurities, securitySchemes);
                }

                if (authMethods != null && !authMethods.isEmpty()) {
                    codegenOperation.authMethods = config.fromSecurity(authMethods);
                    codegenOperation.getVendorExtensions().put(CodegenConstants.HAS_AUTH_METHODS_EXT_NAME, Boolean.TRUE);
                }
            } catch (Exception ex) {
                String msg = "Could not process operation:\n" //
                        + "  Tag: " + tag + "\n"//
                        + "  Operation: " + operation.getOperationId() + "\n" //
                        + "  Resource: " + httpMethod + " " + resourcePath + "\n"//
                       // + "  Definitions: " + swagger.getDefinitions() + "\n"  //
                        + "  Exception: " + ex.getMessage();
                throw new RuntimeException(msg, ex);
            }
        }

    }

    private static String generateParameterId(Parameter parameter) {
        return parameter.getName() + ":" + parameter.getIn();
    }


    private Map<String, Object> processOperations(CodegenConfig config, String tag, List<CodegenOperation> ops, List<Object> allModels) {
        Map<String, Object> operations = new HashMap<>();
        Map<String, Object> objs = new HashMap<>();
        objs.put("classname", config.toApiName(tag));
        objs.put("pathPrefix", config.toApiVarName(tag));

        // check for operationId uniqueness
        Set<String> opIds = new HashSet<>();
        int counter = 0;
        for (CodegenOperation op : ops) {
            String opId = op.nickname;
            if (opIds.contains(opId)) {
                counter++;
                op.nickname += "_" + counter;
            }
            opIds.add(opId);
        }
        objs.put("operation", ops);

        operations.put("operations", objs);
        operations.put("package", config.apiPackage());


        Set<String> allImports = new TreeSet<>();
        for (CodegenOperation op : ops) {
            allImports.addAll(op.imports);
        }

        List<Map<String, String>> imports = new ArrayList<>();
        for (String nextImport : allImports) {
            Map<String, String> im = new LinkedHashMap<>();
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null) {
                im.put("import", mapping);
                imports.add(im);
            }
        }

        operations.put("imports", imports);

        // add a flag to indicate whether there's any {{import}}
        if (imports.size() > 0) {
            operations.put("hasImport", true);
        }
        config.postProcessOperations(operations);
        config.postProcessOperationsWithModels(operations, allModels);
        if (objs.size() > 0) {
            List<CodegenOperation> os = (List<CodegenOperation>) objs.get("operation");

            if (os != null && os.size() > 0) {
                CodegenOperation op = os.get(os.size() - 1);
                op.getVendorExtensions().put(CodegenConstants.HAS_MORE_EXT_NAME, Boolean.FALSE);
            }
        }
        return operations;
    }


    private Map<String, Object> processModels(CodegenConfig config, Map<String, Schema> definitions, Map<String, Schema> allDefinitions) {
        Map<String, Object> objs = new HashMap<>();
        objs.put("package", config.modelPackage());
        List<Object> models = new ArrayList<>();
        Set<String> allImports = new LinkedHashSet<>();
        for (String key : definitions.keySet()) {
            Schema schema = definitions.get(key);
            CodegenModel cm = config.fromModel(key, schema, allDefinitions);
            Map<String, Object> mo = new HashMap<>();
            mo.put("model", cm);
            mo.put("schema", schema);
            mo.put("importPath", config.toModelImport(cm.classname));
            /**
            if (cm.vendorExtensions.containsKey("oneOf-model")) {
                CodegenModel oneOfModel = (CodegenModel) cm.vendorExtensions.get("oneOf-model");
                mo.put("oneOf-model", oneOfModel);
            }
            if (cm.vendorExtensions.containsKey("anyOf-model")) {
                CodegenModel anyOfModel = (CodegenModel) cm.vendorExtensions.get("anyOf-model");
                mo.put("anyOf-model", anyOfModel);
            }
            */
            models.add(mo);

            allImports.addAll(cm.imports);
        }
        objs.put("models", models);
        Set<String> importSet = new TreeSet<>();
        for (String nextImport : allImports) {
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
            // add instantiation types
            mapping = config.instantiationTypes().get(nextImport);
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
        }
        List<Map<String, String>> imports = new ArrayList<>();
        for(String s: importSet) {
            Map<String, String> item = new HashMap<>();
            item.put("import", s);
            imports.add(item);
        }
        objs.put("imports", imports);
        config.postProcessModels(objs);
        return objs;
    }

    private Map<String, Object> processModel(CodegenModel codegenModel, CodegenConfig config, Map<String, Schema> allDefinitions) {
        Map<String, Object> objs = new HashMap<>();
        objs.put("package", config.modelPackage());
        List<Object> models = new ArrayList<>();

        Map<String, Object> modelObject = new HashMap<>();
        modelObject.put("model", codegenModel);
        modelObject.put("importPath", config.toModelImport(codegenModel.classname));

        Set<String> allImports = new LinkedHashSet<>();
        allImports.addAll(codegenModel.imports);
        models.add(modelObject);

        objs.put("models", models);
        Set<String> importSet = new TreeSet<>();
        for (String nextImport : allImports) {
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
            // add instantiation types
            mapping = config.instantiationTypes().get(nextImport);
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
        }
        List<Map<String, String>> imports = new ArrayList<>();
        for(String s: importSet) {
            Map<String, String> item = new HashMap<>();
            item.put("import", s);
            imports.add(item);
        }
        objs.put("imports", imports);
        config.postProcessModels(objs);
        return objs;
    }

    private Map<String, SecurityScheme> getAuthMethods(List<SecurityRequirement> securities, Map<String, SecurityScheme> securitySchemes) {
        if (securities == null || (securitySchemes == null || securitySchemes.isEmpty())) {
            return null;
        }
        final Map<String, SecurityScheme> authMethods = new HashMap<>();
        for (SecurityRequirement requirement : securities) {
            for (String key : requirement.keySet()) {
                SecurityScheme securityScheme = securitySchemes.get(key);
                if (securityScheme != null) {
                    authMethods.put(key, securityScheme);
                }
            }
        }
        return authMethods;
    }

    private Boolean getCustomOptionBooleanValue(String option) {
        List<CodegenArgument> languageArguments = config.getLanguageArguments();
        if (languageArguments == null || languageArguments.isEmpty()) {
            return null;
        }
        Optional<CodegenArgument> optionalCodegenArgument = languageArguments.stream()
                .filter(argument -> option.equalsIgnoreCase(argument.getOption()))
                .findFirst();

        if (!optionalCodegenArgument.isPresent()) {
            return null;
        }
        return Boolean.valueOf(optionalCodegenArgument.get().getValue());
    }

    protected void processSecurityProperties (Map<String, Object> bundle) {
        final Map<String, SecurityScheme> securitySchemeMap = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
        final List<CodegenSecurity> authMethods = config.fromSecurity(securitySchemeMap);
        if (authMethods != null && !authMethods.isEmpty()) {
            bundle.put("authMethods", authMethods);
            bundle.put("hasAuthMethods", true);
        }
    }

}
