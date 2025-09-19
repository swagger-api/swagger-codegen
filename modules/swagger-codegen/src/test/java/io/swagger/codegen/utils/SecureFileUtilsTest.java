package io.swagger.codegen.utils;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.AfterMethod;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Unit tests for SecureFileUtils class.
 * Tests path validation and security checks for preventing path traversal attacks.
 */
public class SecureFileUtilsTest {

    private Path tempDir;
    private File validFile;
    private File validDirectory;

    @BeforeMethod
    public void setUp() throws IOException {
        tempDir = Files.createTempDirectory("secure-file-utils-test");
        validFile = tempDir.resolve("validfile.txt").toFile();
        validDirectory = tempDir.resolve("validdir").toFile();

        if (!validFile.createNewFile()) {
            throw new IOException("Failed to create test file");
        }
        if (!validDirectory.mkdir()) {
            throw new IOException("Failed to create test directory");
        }
    }

    @AfterMethod
    public void tearDown() {
        deleteDirectoryRecursively(tempDir.toFile());
    }

    @Test
    public void testValidatePathWithValidFile() {
        try {
            SecureFileUtils.validatePath(validFile);
        } catch (Exception e) {
            Assert.fail("Valid file should not throw exception: " + e.getMessage());
        }
    }

    @Test
    public void testValidatePathWithValidDirectory() {
        try {
            SecureFileUtils.validatePath(validDirectory);
        } catch (Exception e) {
            Assert.fail("Valid directory should not throw exception: " + e.getMessage());
        }
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void testValidatePathWithNullFile() {
        SecureFileUtils.validatePath((File) null);
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testValidatePathWithFileContainingDotDotInAbsolutePath() throws IOException {
        File parentDir = new File(tempDir.toFile(), "parent");
        File childDir = new File(parentDir, "child");
        File dotDotDir = new File(childDir, "..");
        File targetFile = new File(dotDotDir, "file.txt");

        if (!parentDir.mkdirs()) {
            throw new IOException("Failed to create parent directory");
        }
        if (!childDir.mkdirs()) {
            throw new IOException("Failed to create child directory");
        }

        SecureFileUtils.validatePath(targetFile);
    }

    @Test
    public void testValidatePathWithValidStringPath() {
        String validPath = validFile.getAbsolutePath();

        try {
            SecureFileUtils.validatePath(validPath);
        } catch (Exception e) {
            Assert.fail("Valid string path should not throw exception: " + e.getMessage());
        }
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void testValidatePathWithNullString() {
        SecureFileUtils.validatePath((String) null);
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void testValidatePathWithEmptyString() {
        SecureFileUtils.validatePath("");
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testValidatePathWithParentTraversalString() {
        SecureFileUtils.validatePath("../../../etc/passwd");
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testValidatePathWithBackwardSlashTraversal() {
        SecureFileUtils.validatePath("..\\..\\windows\\system32");
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testValidatePathWithMixedTraversal() {
        SecureFileUtils.validatePath("legitimate/path/../../../sensitive/file");
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testValidatePathWithNullByteInString() {
        SecureFileUtils.validatePath("normalfile.txt\0hiddenfile.exe");
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testValidatePathWithMultipleNullBytes() {
        SecureFileUtils.validatePath("file\0with\0null\0bytes.txt");
    }

    @Test
    public void testValidatePathWithCurrentDirectoryReference() {
        try {
            SecureFileUtils.validatePath("./currentdir/file.txt");
        } catch (Exception e) {
            Assert.fail("Current directory reference should not throw exception: " + e.getMessage());
        }
    }

    @Test
    public void testValidatePathWithFileNameContainingDotsOnly() {
        try {
            SecureFileUtils.validatePath("file.name.with.dots.txt");
        } catch (Exception e) {
            Assert.fail("Filename with dots should not throw exception: " + e.getMessage());
        }
    }

    private void deleteDirectoryRecursively(File dir) {
        if (dir.exists()) {
            File[] files = dir.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        deleteDirectoryRecursively(file);
                    } else {
                        if (!file.delete()) {
                            System.err.println("Failed to delete file: " + file.getAbsolutePath());
                        }
                    }
                }
            }
            if (!dir.delete()) {
                System.err.println("Failed to delete directory: " + dir.getAbsolutePath());
            }
        }
    }

}
