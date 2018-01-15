package io.swagger.codegen.languages;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class KotlinServerCodegen extends AbstractKotlinCodegen {
    static Logger LOGGER = LoggerFactory.getLogger(KotlinServerCodegen.class);

    public static final String DEFAULT_LIBRARY = "ktor";

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "kotlin-server";
    }

    public String getHelp() {
        return "Generates a kotlin server.";
    }

    /**
     * Constructs an instance of `KotlinServerCodegen`.
     */
    public KotlinServerCodegen() {
        super();

        artifactId = "kotlin-server";
        packageName = "io.swagger.server";
        outputFolder = "generated-code" + File.separator + "kotlin-server";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        embeddedTemplateDir = templateDir = "kotlin-server";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        supportedLibraries.put("ktor", "ktor framework");

        // TODO: Configurable server engine. Defaults to netty in build.gradle.
        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        Boolean generateApis = additionalProperties.containsKey(CodegenConstants.GENERATE_APIS) && (Boolean)additionalProperties.get(CodegenConstants.GENERATE_APIS);
        String packageFolder = (sourceFolder + File.separator + packageName).replace(".", File.separator);
        String resourcesFolder = "src/main/resources"; // not sure this can be user configurable.

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));

        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", File.separator);

        supportingFiles.add(new SupportingFile("AppMain.kt.mustache", packageFolder, "AppMain.kt"));

        if (generateApis) {
            supportingFiles.add(new SupportingFile("Paths.kt.mustache", packageFolder, "Paths.kt"));
        }

        supportingFiles.add(new SupportingFile("application.conf.mustache", resourcesFolder, "application.conf"));
        supportingFiles.add(new SupportingFile("logback.xml", resourcesFolder, "logback.xml"));
    }
}
