package io.swagger.v3.generator.online;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenType;
import io.swagger.codegen.v3.service.GeneratorService;
import io.swagger.v3.core.util.Yaml;
import io.swagger.codegen.v3.service.GenerationRequest;
import io.swagger.v3.generator.model.LanguageOptions;
import io.swagger.v3.generator.util.ZipUtil;
import io.swagger.oas.inflector.models.RequestContext;
import io.swagger.oas.inflector.models.ResponseContext;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.parser.util.ClasspathHelper;
import io.swagger.v3.parser.util.RemoteUrl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.function.Function;
import java.util.function.Predicate;

public class GeneratorController {

    static Logger LOGGER = LoggerFactory.getLogger(GeneratorController.class);

    static Map<CodegenType, List<String>> TYPES = new LinkedHashMap<>();
    static Map<io.swagger.codegen.CodegenType, List<String>> TYPESV2 = new LinkedHashMap<>();

    private static ObjectMapper yamlMapper = new ObjectMapper(new YAMLFactory());
    private static LanguageOptions hiddenOptions;
    private static String HIDDEN_OPTIONS_CONFIG_FILE = "hiddenOptions.yaml";
    private static String PROP_HIDDEN_OPTIONS_PATH = "HIDDEN_OPTIONS_PATH";
    private static String PROP_HIDDEN_OPTIONS = "HIDDEN_OPTIONS";

    private static LanguageOptions availableOptions;
    private static String AVAILABLE_OPTIONS_CONFIG_FILE = "availableOptions.yaml";
    private static String PROP_AVAILABLE_OPTIONS_PATH = "AVAILABLE_OPTIONS_PATH";
    private static String PROP_AVAILABLE_OPTIONS = "AVAILABLE_OPTIONS";

    static {

        hiddenOptions = loadLanguageOptions(PROP_HIDDEN_OPTIONS, PROP_HIDDEN_OPTIONS_PATH, HIDDEN_OPTIONS_CONFIG_FILE);
        availableOptions = loadLanguageOptions(PROP_AVAILABLE_OPTIONS, PROP_AVAILABLE_OPTIONS_PATH, AVAILABLE_OPTIONS_CONFIG_FILE);

        final ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class);

        loader.forEach(config -> {
            boolean isServer = CodegenType.SERVER.equals(config.getTag());
            if (shouldProcess(isServer, true, config.getName())) {
                List<String> typeLanguages = TYPES.get(config.getTag());
                if (typeLanguages == null) {
                    typeLanguages = new ArrayList<>();
                    TYPES.put(config.getTag(), typeLanguages);
                }
                typeLanguages.add(config.getName());
            }
        });

        TYPES.forEach((k, v) -> Collections.sort(v, String.CASE_INSENSITIVE_ORDER));

        final ServiceLoader<io.swagger.codegen.CodegenConfig> loaderV2 = ServiceLoader.load(io.swagger.codegen.CodegenConfig.class);

        loaderV2.forEach(config -> {
            boolean isServer = io.swagger.codegen.CodegenType.SERVER.equals(config.getTag());
            if (shouldProcess(isServer, false, config.getName())) {
                List<String> typeLanguages = TYPESV2.get(config.getTag());
                if (typeLanguages == null) {
                    typeLanguages = new ArrayList<>();
                    TYPESV2.put(config.getTag(), typeLanguages);
                }
                typeLanguages.add(config.getName());
            }
        });
        TYPESV2.forEach((k, v) -> Collections.sort(v, String.CASE_INSENSITIVE_ORDER));
    }

    public static LanguageOptions loadLanguageOptions(String languageOptionsKey, String languageOptionsPathKey, String languageOptionsFileKey) {
        LanguageOptions options = null;
        String languages = System.getProperty(languageOptionsKey);
        if (StringUtils.isNotBlank(languages)) {
            options = loadLanguageOptionsFromEnv(languages);
        }
        if (options == null) {
            String languagesPath = System.getProperty(languageOptionsPathKey);
            if (StringUtils.isNotBlank(languagesPath)) {
                options = loadLanguageOptions(languagesPath);
            }
            if (options == null) {
                InputStream inputStream = GeneratorController.class.getClassLoader().getResourceAsStream(languageOptionsFileKey);
                try {
                    options = yamlMapper.readValue(inputStream, LanguageOptions.class);
                } catch (Exception e) {
                    LOGGER.info("Failed to parse language options configuration file {}", languageOptionsFileKey, e);
                    return LanguageOptions.getEmpty();
                }

            }
        }
        if (options != null) {
            return options;
        }
        return LanguageOptions.getEmpty();
    }

    public static LanguageOptions loadLanguageOptionsFromEnv(String csv) {
        try {
            LanguageOptions options = new LanguageOptions();
            String[] sections =csv.split("\\|");
            for (String section: sections) {
                String [] keyval = section.split("\\:");
                switch (keyval[0]) {
                    case "clients":
                        options.getClients().addAll(Arrays.asList(keyval[1].split("\\,")));
                        break;
                    case "clientsV3":
                        options.getClientsV3().addAll(Arrays.asList(keyval[1].split("\\,")));
                        break;
                    case "servers":
                        options.getServers().addAll(Arrays.asList(keyval[1].split("\\,")));
                        break;
                    case "serversV3":
                        options.getServersV3().addAll(Arrays.asList(keyval[1].split("\\,")));
                        break;
                    default:
                }
            }
            return options;
        } catch (Exception e) {
            LOGGER.info("Failed to parse language options. String {}", csv, e);
            return null;
        }
    }
    public static LanguageOptions loadLanguageOptions(String location) {
        try {
            String data = "";
            location = location.replaceAll("\\\\", "/");
            if (location.toLowerCase().startsWith("http")) {
                data = RemoteUrl.urlToString(location, null);
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
            return yamlMapper.readValue(data, LanguageOptions.class);
        } catch (Exception e) {
            LOGGER.info("Failed to parse language options. Path {}", location, e);
            return null;
        }
    }

    @Deprecated
    public ResponseContext clientLanguages(RequestContext requestContext, String version, Boolean clientOnly) {
        List<String> clientAndDoc = new ArrayList<>();
        if ("V2".equals(version)) {
            clientAndDoc.addAll(TYPESV2.get(io.swagger.codegen.CodegenType.CLIENT));
            if (!Boolean.TRUE.equals(clientOnly)) {
                clientAndDoc.addAll(TYPESV2.get(io.swagger.codegen.CodegenType.DOCUMENTATION));
            }
            Collections.sort(clientAndDoc, String.CASE_INSENSITIVE_ORDER);

            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(clientAndDoc);

        }
        clientAndDoc.addAll(TYPES.get(CodegenType.CLIENT));
        if (!Boolean.TRUE.equals(clientOnly)) {
            clientAndDoc.addAll(TYPES.get(CodegenType.DOCUMENTATION));
        }
        Collections.sort(clientAndDoc, String.CASE_INSENSITIVE_ORDER);

        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(clientAndDoc);

    }

    @Deprecated
    public ResponseContext serverLanguages(RequestContext requestContext, String version) {
        if ("V2".equals(version)) {
            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(TYPESV2.get(io.swagger.codegen.CodegenType.SERVER));

        }
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(TYPES.get(CodegenType.SERVER));
    }

    @Deprecated
    public ResponseContext documentationLanguages(RequestContext requestContext, String version) {
        if ("V2".equals(version)) {
            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(TYPESV2.get(io.swagger.codegen.CodegenType.DOCUMENTATION));

        }
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(TYPES.get(CodegenType.DOCUMENTATION));
    }

    public ResponseContext languages(RequestContext requestContext, String type, String version) {
        if ("V2".equals(version)) {
            io.swagger.codegen.CodegenType codegenType = io.swagger.codegen.CodegenType.forValue(type);
            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(TYPESV2.get(codegenType));

        }
        CodegenType codegenTypeV3 = CodegenType.forValue(type);
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(TYPES.get(codegenTypeV3));
    }

    public ResponseContext languagesMulti(RequestContext requestContext, List<String> types, String version) {
        final List<String> languages = new ArrayList<>();
        if ("V2".equals(version)) {
            types.forEach(s -> {
                List<String> typeLanguages = TYPESV2.get(io.swagger.codegen.CodegenType.forValue(s));
                if (typeLanguages != null) {
                    languages.addAll(typeLanguages);
                }
            });
            Collections.sort(languages, String.CASE_INSENSITIVE_ORDER);
            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(languages);

        }
        types.forEach(s -> {
            List<String> typeLanguages = TYPES.get(CodegenType.forValue(s));
            if (typeLanguages != null) {
                languages.addAll(typeLanguages);
            }
        });
        Collections.sort(languages, String.CASE_INSENSITIVE_ORDER);
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(languages);
    }

    public ResponseContext listOptions(RequestContext requestContext, String language, String version) {

        if ("V2".equals(version)) {
            io.swagger.codegen.CodegenConfig config = null;
            try {
                config = CodegenConfigLoader.forName(language);
            } catch (Exception e) {
                String msg = String.format("Unsupported target %s supplied.",
                        language);
                LOGGER.error(msg, e);
                return new ResponseContext()
                        .status(400)
                        .contentType(MediaType.TEXT_PLAIN)
                        .entity(msg);
            }
            Map<String, CliOption> map = new LinkedHashMap<>();
            for (CliOption option : config.cliOptions()) {
                map.put(option.getOpt(), option);
            }
            if (!map.isEmpty()) {
                return new ResponseContext().status(200).entity(map);
            } else {
                return new ResponseContext().status(404);
            }
        } else {
            CodegenConfig config = null;
            try {
                config = io.swagger.codegen.v3.CodegenConfigLoader.forName(language);
            } catch (Exception e) {
                String msg = String.format("Unsupported target %s supplied.",
                        language);
                LOGGER.error(msg, e);
                return new ResponseContext()
                        .status(400)
                        .contentType(MediaType.TEXT_PLAIN)
                        .entity(msg);
            }
            Map<String, io.swagger.codegen.v3.CliOption> map = new LinkedHashMap<>();
            for (io.swagger.codegen.v3.CliOption option : config.cliOptions()) {
                map.put(option.getOpt(), option);
            }
            if (!map.isEmpty()) {
                return new ResponseContext().status(200).entity(map);
            } else {
                return new ResponseContext().status(404);
            }
        }
    }

    public ResponseContext generateFromURL(RequestContext context, String codegenOptionsURL) {
        final String content;
        LOGGER.debug("generateFromURL start - " + codegenOptionsURL);
        try {
            content = RemoteUrl.urlToString(codegenOptionsURL, null);
        } catch (Exception e) {
            String msg = "Unable to read URL: " + codegenOptionsURL;
            LOGGER.error(msg, e);
            return new ResponseContext()
                    .status(400)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity(msg);
        }

        if (StringUtils.isBlank(content)) {
            String msg = "Empty content found in URL: " + codegenOptionsURL;
            LOGGER.error(msg);
            return new ResponseContext()
                    .status(404)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity(msg);
        }

        GenerationRequest generationRequest = null;
        try {
            generationRequest = Json.mapper().readValue(content, GenerationRequest.class);
        } catch (Exception e) {
            try {
                generationRequest = Yaml.mapper().readValue(content, GenerationRequest.class);
            } catch (Exception ee) {
                String msg = "Could not process content of URL: " + codegenOptionsURL;
                LOGGER.error(msg, ee);
                return new ResponseContext()
                        .status(400)
                        .contentType(MediaType.TEXT_PLAIN)
                        .entity(msg);
            }
        }

        ResponseContext responseContext = generate(context, generationRequest);
        LOGGER.debug("generateFromURL end - " + codegenOptionsURL);
        return responseContext;

    }

    private String requestLog(GenerationRequest generationRequest) {
        final int maxLength = 41;
        StringBuffer requestLog = new StringBuffer();
        requestLog.append("lang: ");
        requestLog.append(generationRequest.getLang());
        requestLog.append(", ");
        requestLog.append("version: ");
        requestLog.append(generationRequest.getCodegenVersion().name());
        requestLog.append(", ");
        requestLog.append("specURL: ");
        requestLog.append(generationRequest.getSpecURL());
        requestLog.append(", ");
        if (generationRequest.getSpec() != null) {
            String spec = null;
            if (generationRequest.getSpec() instanceof String && StringUtils.isNotBlank((String) generationRequest.getSpec())) {
                if (((String) generationRequest.getSpec()).length() > maxLength) {
                    spec = ((String) generationRequest.getSpec()).substring(0, 40);
                } else {
                    spec = ((String) generationRequest.getSpec());
                }
                requestLog.append("spec: ");
                requestLog.append(spec);
                requestLog.append(", ");
            } else {
                try {
                    spec = Json.pretty(generationRequest.getSpec());
                    if (spec.length() > maxLength) {
                        spec = spec.substring(0, 40);
                    }
                    requestLog.append("spec: ");
                    requestLog.append(spec);
                    requestLog.append(", ");
                } catch (Exception e) {
                    requestLog.append("spec: ");
                    requestLog.append(spec);
                    requestLog.append(", ");
                }
            }
        }
        if (generationRequest.getOptions() != null) {
            if (StringUtils.isNotBlank(generationRequest.getOptions().getLibrary())) {
                requestLog.append("lib: ");
                requestLog.append(generationRequest.getOptions().getLibrary());
            }
        }
        return requestLog.toString();
    }

    public ResponseContext generate(RequestContext context, GenerationRequest generationRequest) {

        String requestLog = requestLog(generationRequest);
        LOGGER.debug("generate start - " + requestLog);
        File outputRootFolder = getTmpFolder();
        String destPath = null;

        if(generationRequest != null && generationRequest.getOptions() != null) {
            Object destPathObj = generationRequest.getOptions().getAdditionalProperties().get("outputFolder");
            if (destPathObj != null && destPathObj instanceof String) {
                destPath = (String)destPathObj;
            }
        }
        if(destPath == null) {
            destPath = "";
        }

        // remove double slashes
        destPath.replaceAll("//", "/");

        if(destPath.indexOf("..") != -1) {
            return new ResponseContext()
                    .status(400)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity("Illegal output folder");
        }

        // remove leading slash (will typically not hurt)
        if(destPath.indexOf("/") == 0 && destPath.length() > 1) {
            destPath = destPath.substring(1);
        }

        // destPath is where the files are written, relative to output folder
        LOGGER.info("using destination path " + destPath);

        File outputContentFolder = null;
        if (!StringUtils.isBlank(destPath.trim())) {
            outputContentFolder = new File(outputRootFolder, destPath);
        } else {
            outputContentFolder = outputRootFolder;
        }
        generationRequest.getOptions().setOutputDir(outputContentFolder.getAbsolutePath());
        File outputFile = new File(getTmpFolder(), generationRequest.getLang() + "-bundle.zip");

        LOGGER.info("file zip file: " + outputFile.getAbsolutePath());

        ResponseContext responseContext = generate(generationRequest, outputRootFolder, outputContentFolder, outputFile);
        LOGGER.debug("generate end - " + requestLog);
        return responseContext;

    }

    protected static File getTmpFolder() {
        try {
            File outputFolder = File.createTempFile("codegen-", "-tmp");
            outputFolder.delete();
            outputFolder.mkdir();
            outputFolder.deleteOnExit();
            return outputFolder;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    private ResponseContext generate(GenerationRequest generationRequest, File outputRootFolder, File outputContentFolder, File outputFile) {
        GeneratorService generatorService = new GeneratorService();
        try {
            generatorService.generationRequest(generationRequest);
        } catch (Exception e) {
            String msg = "Error processing generation request: " + e.getMessage();
            LOGGER.error(msg, e);
            return new ResponseContext()
                    .status(400)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity(msg);
        }

        final List<File> files;
        try {
            files = generatorService.generate();
        } catch (Exception e) {
            String msg = String.format("Error generating `%s` code : %s", generationRequest.getLang(), e.getMessage());
            LOGGER.error(msg, e);
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity(msg);
        }
        if (files.size() > 0) {
            if (generationRequest.getType() == null) {
                String lang = generationRequest.getLang();
                if (GenerationRequest.CodegenVersion.V2.equals(generationRequest.getCodegenVersion())) {
                    TYPESV2
                            .entrySet().stream()
                            .filter(e -> {
                                return e.getValue().contains(lang);
                            })
                            .findFirst()
                            .ifPresent(e -> {
                                generationRequest.type(GenerationRequest.Type.fromValue(e.getKey().toValue()));
                            });
                } else {
                    TYPES
                            .entrySet().stream()
                            .filter(e -> {
                                return e.getValue().contains(lang);
                            })
                            .findFirst()
                            .ifPresent(e -> {
                                generationRequest.type(GenerationRequest.Type.fromValue(e.getKey().toValue()));
                            });
                }
                if (generationRequest.getType() == null) {
                    generationRequest.type(GenerationRequest.Type.CLIENT);
                }
            }
            return downloadFile(outputRootFolder, outputContentFolder, outputFile, generationRequest.getLang(), generationRequest.getType());
        } else {
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity("A target generation was attempted, but no files were created");
        }


    }
    private ResponseContext downloadFile(File outputRootFolder, File outputContentFolder, File outputFile, String lang, GenerationRequest.Type type) {

        final ZipUtil zipUtil = new ZipUtil();
        try {
            zipUtil.compressFiles(Arrays.asList(outputRootFolder.listFiles()), outputFile.getAbsolutePath());
        } catch (IOException e) {
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity("Could not generate zip file.");
        }

        if (!outputFile.exists()) {
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity("File was not generated.");
        }
        byte[] bytes = null;
        try {
            bytes = FileUtils.readFileToByteArray(outputFile);
        } catch (IOException ex) {
            LOGGER.error("Error converting file to bytes.", ex);
            bytes = null;
        }
        try {
            FileUtils.deleteDirectory(outputRootFolder);
        } catch (IOException ex) {
            LOGGER.error("Could not delete files.", ex);
        }
        if (bytes != null) {
            final String friendlyName = lang + "-" + type.getTypeName();
            return new ResponseContext().status(200)
                    .entity(bytes)
                    .contentType(MediaType.APPLICATION_OCTET_STREAM_TYPE)
                    .header("Content-Disposition", String.format("attachment; filename=\"%s-generated.zip\"", friendlyName))
                    .header("Accept-Range", "bytes")
                    .header("Content-Length", String.valueOf(bytes.length));
        }
        return new ResponseContext()
                .status(500)
                .contentType(MediaType.TEXT_PLAIN)
                .entity("Could not generate files.");
    }

    private static boolean shouldProcess(boolean isServer, boolean isV3, String name){
        if(isServer){
            return shouldProcessServer(isV3, name);
        }else{
            return shouldProcessClient(isV3, name);
        }
    }

    private static boolean shouldProcessServer(boolean isV3, String name){

        if(isServerHidden(isV3, name)){
            return false;
        }

        if(availableOptions.wereValuesSpecified() && !isServerAvailable(isV3, name)){
            return false;
        }

        return true;
    }

    private static boolean shouldProcessClient(boolean isV3, String name){
        if(isClientHidden(isV3, name)){
            return false;
        }

        if(availableOptions.wereValuesSpecified() && !isClientAvailable(isV3, name)){
            return false;
        }

        return true;
    }

    private static boolean isClientAvailable(boolean isV3, String name){
        if(isV3){
            return availableOptions.isClientV3(name);
        }else{
            return availableOptions.isClient(name);
        }
    }

    private static boolean isClientHidden(boolean isV3, String name){
        if (isV3) {
            return hiddenOptions.isClientV3(name);
        }else {
            return hiddenOptions.isClient(name);
        }
    }

    private static boolean isServerAvailable(boolean isV3, String name){
        if(isV3){
            return availableOptions.isServerV3(name);
        }else{
            return availableOptions.isServer(name);
        }
    }

    private static boolean isServerHidden(boolean isV3, String name){
        if (isV3) {
            return hiddenOptions.isServerV3(name);
        }else {
            return hiddenOptions.isServer(name);
        }
    }


}
