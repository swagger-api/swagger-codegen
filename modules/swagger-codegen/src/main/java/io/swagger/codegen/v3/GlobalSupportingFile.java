package io.swagger.codegen.v3;

public class GlobalSupportingFile extends SupportingFile {

    GlobalSupportingFile(String templateFile, String folder, String destinationFilename) {
        super(templateFile, folder, destinationFilename);
    }

    GlobalSupportingFile(String templateFile, String destinationFilename) {
        super(templateFile, destinationFilename);
    }
}
