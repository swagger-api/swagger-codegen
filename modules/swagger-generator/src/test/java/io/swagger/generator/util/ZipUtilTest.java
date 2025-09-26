package io.swagger.generator.util;

import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;

public class ZipUtilTest {

    @Test(expectedExceptions = SecurityException.class)
    public void testCompressFilesWithPathTraversal() throws Exception {
        ZipUtil zipUtil = new ZipUtil();

        Path tempFile = Files.createTempFile("test", ".txt");
        Files.write(tempFile, "test content".getBytes());

        zipUtil.compressFiles(Collections.singletonList(tempFile.toFile()), "../../../etc/passwd.zip");
    }
}
