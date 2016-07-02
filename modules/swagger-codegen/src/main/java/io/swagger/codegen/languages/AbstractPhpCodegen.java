package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.*;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.HashSet;
import java.util.regex.Matcher;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractPhpCodegen extends DefaultCodegen implements CodegenConfig {
    // @SuppressWarnings("hiding")
    // static Logger LOGGER = LoggerFactory.getLogger(AbstractPhpCodegen.class);

    // public static final String VARIABLE_NAMING_CONVENTION = "variableNamingConvention";
    // public static final String PACKAGE_PATH = "packagePath";
    // public static final String SRC_BASE_PATH = "srcBasePath";
    // public static final String COMPOSER_VENDOR_NAME = "composerVendorName";
    // public static final String COMPOSER_PROJECT_NAME = "composerProjectName";
    // protected String invokerPackage = "Swagger\\Client";
    // protected String composerVendorName = null;
    // protected String composerProjectName = null;
    // protected String packagePath = "SwaggerClient-php";
    // protected String artifactVersion = null;
    // protected String srcBasePath = "lib";
    // protected String testBasePath = "test";
    // protected String docsBasePath = "docs";
    // protected String apiDirName = "Api";
    // protected String modelDirName = "Model";
    // protected String variableNamingConvention= "snake_case";
    // protected String apiDocPath = docsBasePath + "/" + apiDirName;
    // protected String modelDocPath = docsBasePath + "/" + modelDirName;

    public AbstractPhpCodegen() {
        super();

    }

   

}
