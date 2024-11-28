package io.swagger.codegen.v3.cli.cmd;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class Version implements Runnable {

    private static final Logger LOGGER = LoggerFactory.getLogger(Meta.class);

    public static final String SWAGGER_CODEGEN_VERSION = "swagger-codegen.version";
    public static final String SWAGGER_CODEGEN_GENERATORS_VERSION = "swagger-codegen.generators.version";

    private static final String VERSION_PLACEHOLDER = "${project.version}";

    private static final String UNREADABLE_VERSION = "unreadable";
    private static final String UNSET_VERSION = "unset";
    private static final String UNKNOWN_VERSION = "unknown";

    public static String readVersionFromResources() {
        return readVersionFromResources(SWAGGER_CODEGEN_VERSION);
    }

    public static String readVersionFromResources(String key) {
        Properties versionProperties = new Properties();
        try (InputStream is = Version.class.getResourceAsStream("/version.properties")) {
            versionProperties.load(is);
        } catch (IOException ex) {
            LOGGER.error("Error loading version properties", ex);
            return UNREADABLE_VERSION;
        }

        String version = versionProperties.getProperty(key, UNKNOWN_VERSION).trim();
        if (VERSION_PLACEHOLDER.equals(version)) {
            return UNSET_VERSION;
        } else {
            return version;
        }
    }

    @Override
    public void run() {
        String version = readVersionFromResources();
        System.out.println(version);
    }

}
