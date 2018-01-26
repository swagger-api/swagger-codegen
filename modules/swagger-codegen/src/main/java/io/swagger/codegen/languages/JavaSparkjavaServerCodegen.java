package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.LinkedList;

public class JavaSparkjavaServerCodegen extends AbstractJavaCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";
    private static final char PKG_SEPARATOR = '.';
    static Logger LOGGER = LoggerFactory.getLogger(JavaSparkjavaServerCodegen.class);
    private static String[] BASE_PACKAGE_PARTS = new String[]{"io", "swagger", "server"};

    private static String BASE_PACKAGE_DIR = StringUtils.join(BASE_PACKAGE_PARTS, PKG_SEPARATOR);

//    private static String JAVA_SRC_DIR = StringUtils.join(
//            new String[]{"src", "main", "java"},
//            PKG_SEPARATOR
//    );

    private static String JAVA_PKG_SRC_DIR = PKG_SEPARATOR + BASE_PACKAGE_DIR;

    public JavaSparkjavaServerCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "java-sparkjava";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        embeddedTemplateDir = templateDir = "java-sparkjava";
        apiPackage = javaSrcFolder("api");
        modelPackage = javaSrcFolder("models");
        // TODO: Fill this out.
        // Force Java 8 mode as SparkJava 2.x needs it
        for (CliOption cliOption : new LinkedList<>(cliOptions)) {
            if (cliOption.getOpt().equals(JAVA8_MODE) || cliOption.getOpt().equals(DATE_LIBRARY)) {
                cliOptions.remove(cliOption);
            }
        }
        setDateLibrary("java8");
        setJava8Mode(true);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
    }

    private String javaSrcFolder(String folderName) {
        return JAVA_PKG_SRC_DIR + PKG_SEPARATOR + folderName;
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-sparkjava";
    }

    public String getHelp() {
        return "Generates a java-sparkjava server.";
    }

    public String getApiPackage() {
        return StringUtils.join('.', BASE_PACKAGE_PARTS);
    }

    @Override
    public String testPackage() {
        String testPackage = super.testPackage();
        return testPackage.startsWith(".") ? testPackage.substring(1) : testPackage;
    }

    @Override
    public String modelPackage() {
        String modelPackage = super.modelPackage();
        return modelPackage.startsWith(".") ? modelPackage.substring(1) : modelPackage;
    }

    @Override
    public String apiPackage() {
        String apiPackage = super.apiPackage();
        return apiPackage.startsWith(".") ? apiPackage.substring(1) : apiPackage;
    }
}
