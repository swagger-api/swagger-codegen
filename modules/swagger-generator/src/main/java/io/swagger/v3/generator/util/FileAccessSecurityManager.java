package io.swagger.v3.generator.util;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.security.AccessControlException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FileAccessSecurityManager extends SecurityManager {

    static Logger LOGGER = LoggerFactory.getLogger(FileAccessSecurityManager.class);

    static List<String> allowedDirectories= StringUtils.isBlank(System.getProperty("generatorWriteDirs")) ? new ArrayList<>() : Arrays.asList(System.getProperty("generatorWriteDirs").split(","));

    @Override
    public void checkWrite(String file) {
        super.checkWrite(file);

        if (allowedDirectories.isEmpty()) {
            return;
        }
        if (!StringUtils.isBlank(file)) {
            boolean granted = false;

                for (String dir : allowedDirectories) {
                    try {
                        String dirPath = new File(dir).getCanonicalPath();
                        if (new File(file).getCanonicalPath().startsWith(dirPath)) {
                            granted = true;
                        }
                    } catch (IOException e) {
                        LOGGER.error("Exception getting absolute path for file {} and/or allowed dir ", file, e);
                        throw new SecurityException("Exception getting absolute path for allowed dir " + dir + " and/or file " + file);
                    }

                }
                if (!granted) {
                    LOGGER.error("Blocking attempt to write to not allowed directory for file " + file);
                    throw new AccessControlException("Error writing file to " + file + " as target dir is not allowed");
                }
        }
    }
}
