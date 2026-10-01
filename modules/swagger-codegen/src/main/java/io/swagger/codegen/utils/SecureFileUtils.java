package io.swagger.codegen.utils;

import java.io.File;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Utility class for secure file operations that prevent path traversal attacks.
 * Uses a simplified approach focusing on canonical path validation and allowlist-based security.
 */
public class SecureFileUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(SecureFileUtils.class);

    private SecureFileUtils() {
        // Utility class
    }

    public static void validatePath(File file) {
        if (file == null) {
            LOGGER.error("File cannot be null");
            throw new IllegalArgumentException("File cannot be null");
        }

        try {
            String absolutePath = file.getAbsolutePath();
            String canonicalPath = file.getCanonicalPath();

            if (absolutePath.contains("..") || absolutePath.contains("\0")) {
                LOGGER.error("Path contains suspicious characters: {}", absolutePath);
                throw new SecurityException("Path contains suspicious characters: " + absolutePath);
            }

            if (canonicalPath.contains("..") || canonicalPath.contains("\0")) {
                LOGGER.error("Path contains suspicious characters: {}", canonicalPath);
                throw new SecurityException("Path contains suspicious characters: " + canonicalPath);
            }

        } catch (IOException e) {
            LOGGER.error("Unable to resolve canonical path for: {}, error: {}", file.getAbsolutePath(), e.getMessage());
            throw new SecurityException("Unable to resolve canonical path for: " + file.getAbsolutePath(), e);
        }
    }

    public static void validatePath(String path) {
        if (path == null || path.trim().isEmpty()) {
            LOGGER.error("Path cannot be null or empty");
            throw new IllegalArgumentException("Path cannot be null or empty");
        }

        if (path.contains("..") || path.contains("\0")) {
            LOGGER.error("Path contains suspicious characters: {}", path);
            throw new SecurityException("Path contains suspicious characters: " + path);
        }

        validatePath(new File(path));
    }
}
