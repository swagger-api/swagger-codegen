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

    /**
     * checks if the directory file lists and file content is equal
     * 
     * @param directory
     *            the directory
     * @param compareDirectory
     *            the directory to compare with
     * @param checkFileContent
     *            also compare file content
     * @return true if directory and compareDirectory are equal
     * @throws IOException
     */
    public static boolean isEqualDirectories(Path directory, Path compareDirectory, boolean checkFileContent) throws IOException {
        boolean isRoot = true;
        boolean check = isEverythingInCompareDirectory(directory, compareDirectory, checkFileContent, isRoot);
        boolean checkOpposite = check && isEverythingInCompareDirectory(directory, compareDirectory, checkFileContent, isRoot);
        return check && checkOpposite;

    }

    /**
     * checks if the directory file lists and file content is equal
     * 
     * @param directory
     *            the directory
     * @param compareDirectory
     *            the directory to compare with
     * @param checkFileContent
     *            also compare file content
     * @return true if directory and compareDirectory are equal
     * @throws IOException
     */
    public static boolean isEverythingInCompareDirectory(Path directory, Path compareDirectory, boolean checkFileContent, boolean isRoot)
            throws IOException {

        try {
            LOGGER.info("checking directory " + directory);

            File directoryFile = directory.toFile();
            File compareFile = compareDirectory.toFile();
            LOGGER.debug("directoryFile: " + directoryFile);
            LOGGER.debug("compareFile: " + compareFile);
            
            // check, if there is the same number of files/subdirectories
            File[] directoryFiles = null;
            File[] compareFiles = null;
            if (isRoot) {
                directoryFiles = directoryFile.listFiles(new EclipseFilenameFilter());
                compareFiles = compareFile.listFiles(new EclipseFilenameFilter());
            } else {
                directoryFiles = directoryFile.listFiles();
                compareFiles = compareFile.listFiles();
            }
            
            if (directoryFiles.length == compareFiles.length) {
                return compareDirectoryContents(directory, compareDirectory, checkFileContent);

            } else {
                LOGGER.info("number of files in directory are different " + directoryFiles.length + " vs compareDirectory: " + compareFiles.length);
                return false;
            }

        } catch (IOException e) {
            throw new RuntimeException("Failed to assert that all files are equal", e);
        }
    }

    public static boolean compareDirectoryContents(Path directory, Path compareDirectory, boolean checkFileContent) throws IOException {
        try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(directory)) {

            for (Path directoryFilePath : directoryStream) {

                // search for directoryFile in the compareDirectory
                Path compareFilePath = compareDirectory.resolve(directoryFilePath.getFileName());

                if (compareFilePath != null) {

                    File directoryFile = directoryFilePath.toFile();
                    if (directoryFile.isFile()) {
                        LOGGER.info("checking file " + directoryFilePath);
                        if (checkFileContent && !FileUtils.contentEquals(compareFilePath.toFile(), directoryFile)) {
                            LOGGER.info("files not equal: compare: " + compareFilePath.toFile() + ", directory: " + directoryFilePath.getFileName() + "!");
                            return false;
                        }

                    } else {
                        LOGGER.info("going into recursion with directory " + directoryFilePath);
                        boolean result = isEverythingInCompareDirectory(directoryFilePath, compareFilePath,
                                checkFileContent, false);

                        // if not equal, then cancel compare
                        if (!result) {
                            return false;
                        }
                    }
                } else {
                    LOGGER.info(directoryFilePath.toString() + ": compareFilepath not found");
                    return false;
                }

            }
        }

        return true;
    }

}
