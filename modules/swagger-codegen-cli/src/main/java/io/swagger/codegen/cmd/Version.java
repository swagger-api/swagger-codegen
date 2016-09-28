package io.swagger.codegen.cmd;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import io.airlift.airline.Command;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Command(name = "version", description = "Show version information")
public class Version implements Runnable {

    private static final Logger LOGGER = LoggerFactory.getLogger(Meta.class);

    static Optional<String> readVersionFromResources() {
        Properties versionProperties = new Properties();
        try (InputStream is = Version.class.getResourceAsStream("/version.properties")) {
            versionProperties.load(is);
        } catch (IOException ex) {
            LOGGER.error("Error loading version properties", ex);
            return Optional.absent();
        }
        String version = versionProperties.getProperty("version");
        return Optional.fromNullable(version);
    }

    @Override
    public void run() {
        Optional<String> version = readVersionFromResources();
        Preconditions.checkState(version.isPresent(), "Version properties missing 'version' entry");
        System.out.println(version.get());
    }

}
