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
import io.swagger.v3.generator.model.HiddenOptions;
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

public class GeneratorController {

    static Logger LOGGER = LoggerFactory.getLogger(GeneratorController.class);
    static List<String> CLIENTS = new ArrayList<>();
    static List<String> SERVERS = new ArrayList<>();
    static List<String> CLIENTSV2 = new ArrayList<>();
    static List<String> SERVERSV2 = new ArrayList<>();

    private static ObjectMapper yamlMapper = new ObjectMapper(new YAMLFactory());
    private static ObjectMapper jsonMapper = new ObjectMapper();
    private static HiddenOptions hiddenOptions;
    private static String HIDDEN_OPTIONS_CONFIG_FILE = "hiddenOptions.yaml";
    private static String PROP_HIDDEN_OPTIONS_PATH = "HIDDEN_OPTIONS_PATH";
    private static String PROP_HIDDEN_OPTIONS = "HIDDEN_OPTIONS";

    static {

        hiddenOptions = loadHiddenOptions();
        final ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class);

        loader.forEach(config -> {
            if ((config.getTag().equals(CodegenType.CLIENT) || config.getTag().equals(CodegenType.DOCUMENTATION)) && !hiddenOptions.isHiddenClientV3(config.getName()))  {
                CLIENTS.add(config.getName());
            } else if (config.getTag().equals(CodegenType.SERVER) && !hiddenOptions.isHiddenServerV3(config.getName())) {
                SERVERS.add(config.getName());
            }
        });
        Collections.sort(CLIENTS, String.CASE_INSENSITIVE_ORDER);
        Collections.sort(SERVERS, String.CASE_INSENSITIVE_ORDER);

        final ServiceLoader<io.swagger.codegen.CodegenConfig> loaderV2 = ServiceLoader.load(io.swagger.codegen.CodegenConfig.class);

        loaderV2.forEach(config -> {
            if ((config.getTag().equals(io.swagger.codegen.CodegenType.CLIENT) || config.getTag().equals(io.swagger.codegen.CodegenType.DOCUMENTATION)) && !hiddenOptions.isHiddenClient(config.getName())) {
                CLIENTSV2.add(config.getName());
            } else if (config.getTag().equals(io.swagger.codegen.CodegenType.SERVER) && !hiddenOptions.isHiddenServer(config.getName())) {
                SERVERSV2.add(config.getName());
            }
        });
        Collections.sort(CLIENTSV2, String.CASE_INSENSITIVE_ORDER);
        Collections.sort(SERVERSV2, String.CASE_INSENSITIVE_ORDER);
    }


    public static HiddenOptions loadHiddenOptions() {
        HiddenOptions options = null;
        String hidden = System.getProperty(PROP_HIDDEN_OPTIONS);
        if (StringUtils.isNotBlank(hidden)) {
            options = loadHiddenOptionsFromEnv(hidden);
        }
        if (options == null) {
            String hiddenPath = System.getProperty(PROP_HIDDEN_OPTIONS_PATH);
            if (StringUtils.isNotBlank(hiddenPath)) {
                options = loadHiddenOptions(hiddenPath);
            }
            if (options == null) {
                InputStream inputStream = GeneratorController.class.getClassLoader().getResourceAsStream(HIDDEN_OPTIONS_CONFIG_FILE);
                try {
                    options = yamlMapper.readValue(inputStream, HiddenOptions.class);
                } catch (Exception e) {
                    LOGGER.info("Failed to parse hidden options configuration file {}", HIDDEN_OPTIONS_CONFIG_FILE, e);
                    return HiddenOptions.getEmpty();
                }

            }
        }
        if (options != null) {
            LOGGER.debug("Parsed hidden options config");
            LOGGER.debug("Hidden clients: {}", options.clients());
            LOGGER.debug("Hidden servers: {}", options.servers());
            LOGGER.debug("Hidden clientsV3: {}", options.clientsV3());
            LOGGER.debug("Hidden serversV3: {}", options.serversV3());
            return options;
        }
        return HiddenOptions.getEmpty();
    }

    public static HiddenOptions loadHiddenOptionsFromEnv(String csv) {
        try {
            HiddenOptions options = new HiddenOptions();
            String[] sections =csv.split("\\|");
            for (String section: sections) {
                LOGGER.error(section);
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
            LOGGER.info("Failed to parse hidden options. String {}", csv, e);
            return null;
        }
    }
    public static HiddenOptions loadHiddenOptions(String location) {
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
            return yamlMapper.readValue(data, HiddenOptions.class);
        } catch (Exception e) {
            LOGGER.info("Failed to parse hidden options. Path {}", location, e);
            return null;
        }
    }

    public ResponseContext clientLanguages(RequestContext requestContext, String version) {
        if ("v2".equals(version)) {
            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(CLIENTSV2);

        }
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(CLIENTS);

    }

    public ResponseContext serverLanguages(RequestContext requestContext, String version) {
        if ("v2".equals(version)) {
            return new ResponseContext()
                    .status(Response.Status.OK.getStatusCode())
                    .entity(SERVERSV2);

        }
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(SERVERS);
    }

    public ResponseContext listOptions(RequestContext requestContext, String language, String version) {

        if ("v2".equals(version)) {
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

        return generate(context, generationRequest);

    }

    public ResponseContext generate(RequestContext context, GenerationRequest generationRequest) {
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
        File outputFile = new File(getTmpFolder(), generationRequest.getOptions().getLang() + "-bundle.zip");

        LOGGER.info("file zip file: " + outputFile.getAbsolutePath());

        return generate(generationRequest, outputRootFolder, outputContentFolder, outputFile);

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
            String msg = String.format("Error generating `%s` code : %s", generationRequest.getOptions().getLang(), e.getMessage());
            LOGGER.error(msg, e);
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.TEXT_PLAIN)
                    .entity(msg);
        }
        if (files.size() > 0) {
            return downloadFile(outputRootFolder, outputContentFolder, outputFile, generationRequest.getOptions().getLang(), generationRequest.getType());
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
            zipUtil.compressFiles(Arrays.asList(outputContentFolder.listFiles()), outputFile.getAbsolutePath());
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
}
