package io.swagger.codegen.generators;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComparePathUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(JaxrsSpec_Generator_Petstore.class);

    public static void assertThatAllFilesAreEqual(Path expectedDirectory, Path actualDirectory)
            throws Exception {

        try {
            try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(expectedDirectory)) {
                for (Path expectedFilePath : directoryStream) {

                    // search for expectedFile in the actualDirectory
                    LOGGER.info("checking file " + expectedFilePath);

                    Path actualFilePath = actualDirectory.resolve(expectedFilePath.getFileName());

                    File expectedFile = expectedFilePath.toFile();
                    if (expectedFile.isFile()) {
                        if (!FileUtils.contentEquals(actualFilePath.toFile(), expectedFile)) {
                            throw new Exception("files not equal: actual: " + actualFilePath.toFile() + ", expected: " + expectedFilePath.getFileName() + "!");
                        };
                      
                    } else {
                        LOGGER.info("going into recursion with directory " + expectedFilePath);
                        assertThatAllFilesAreEqual(expectedFilePath, actualFilePath);
                    }
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to assert that all files are equal", e);
        }
    }

}
