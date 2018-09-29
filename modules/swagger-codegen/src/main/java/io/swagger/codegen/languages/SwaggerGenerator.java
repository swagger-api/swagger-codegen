package io.swagger.codegen.languages;

import java.io.File;
import java.util.List;
import java.util.Map;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.models.Model;
import io.swagger.models.properties.Property;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Swagger;
import io.swagger.util.Json;

public class SwaggerGenerator extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(SwaggerGenerator.class);

    public static final String OUTPUT_NAME = "outputFile";

    public static final String SWAGGER_FILENAME_DEFAULT_JSON = "swagger.json";

    protected String outputFile = SWAGGER_FILENAME_DEFAULT_JSON;

    public SwaggerGenerator() {
        super();
        embeddedTemplateDir = templateDir = "swagger";
        outputFolder = "generated-code/swagger";

        cliOptions.add(new CliOption(OUTPUT_NAME,
                "output filename")
                .defaultValue(SWAGGER_FILENAME_DEFAULT_JSON));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "swagger";
    }

    @Override
    public String getHelp() {
        return "Creates a static swagger.json file.";
    }

    @Override
    public void processSwagger(Swagger swagger) {
        String swaggerString = Json.pretty(swagger);

        try {
            String outputFile = outputFolder + File.separator + this.outputFile;
            FileUtils.writeStringToFile(new File(outputFile), swaggerString);
            LOGGER.debug("wrote file to " + outputFile);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(OUTPUT_NAME) && !StringUtils.isBlank((String) additionalProperties.get(OUTPUT_NAME))) {
            setOutputFile((String) additionalProperties.get(OUTPUT_NAME));
        }
    }

    public void setOutputFile(String outputFile) {
        this.outputFile = outputFile;
    }

    @Override
    public String escapeQuotationMark(String input) { 
        // just return the original string
        return input;
    }

    @Override
    protected List<Map<String, String>> getExamples(Map<String, Model> definitions, Map<String, Object> examples, List<String> produces, Object object) {
        if (examples == null || examples.isEmpty()) {
            return null;
        }
        return super.getExamples(definitions, examples, produces, object);
    }

    @Override
    public String escapeUnsafeCharacters(String input) { 
        // just return the original string
        return input;
    }  
}
