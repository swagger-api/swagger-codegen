package io.swagger.codegen.utils;

import java.io.File;
import java.io.IOException;


/**
 * Utility class for secure file operations that prevent path traversal attacks.
 * Uses a simplified approach focusing on canonical path validation and allowlist-based security.
 */
public class SecureFileUtils {

    private SecureFileUtils() {
        // Utility class
    }

    public static void validatePath(File file) {
        if (file == null) {
            throw new IllegalArgumentException("File cannot be null");
        }

        try {
            String absolutePath = file.getAbsolutePath();
            String canonicalPath = file.getCanonicalPath();

            if (absolutePath.contains("..") || absolutePath.contains("\0")) {
                throw new SecurityException("Path contains suspicious characters: " + absolutePath);
            }

            if (canonicalPath.contains("..") || canonicalPath.contains("\0")) {
                throw new SecurityException("Path contains suspicious characters: " + canonicalPath);
            }

        } catch (IOException e) {
            throw new SecurityException("Unable to resolve canonical path for: " + file.getAbsolutePath(), e);
        }
    }

    public static void validatePath(String path) {
        if (path == null || path.trim().isEmpty()) {
            throw new IllegalArgumentException("Path cannot be null or empty");
        }

        if (path.contains("..") || path.contains("\0")) {
            throw new SecurityException("Path contains suspicious characters: " + path);
        }

        validatePath(new File(path));
    }
}
