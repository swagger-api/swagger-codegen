package io.swagger.codegen;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class SupportingFile {
    public String templateFile;
    public String folder;
    public String destinationFilename;

    public SupportingFile(String templateFile, String destinationFilename) {
        this(templateFile, "", destinationFilename);
    }

    public SupportingFile(String templateFile, String folder, String destinationFilename) {
        this.templateFile = templateFile;
        this.folder = folder;
        this.destinationFilename = destinationFilename;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SupportingFile:").append("\n");
        builder.append("\ttemplateFile: ").append(templateFile).append("\n");
        builder.append("\tfolder: ").append(folder).append("\n");
        builder.append("\tdestinationFilename: ").append(destinationFilename).append("\n");

        return builder.toString();
    }

    @Override
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }
}


