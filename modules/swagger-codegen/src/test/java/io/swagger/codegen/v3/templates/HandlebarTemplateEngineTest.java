package io.swagger.codegen.v3.templates;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.stream.Stream;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import io.swagger.codegen.v3.CodegenConstants;
import io.swagger.codegen.v3.generators.DefaultCodegenConfig;

public class HandlebarTemplateEngineTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(HandlebarTemplateEngineTest.class);

    private static final String API_JAVA_CUSOM_EXPECTED = readFileToString(
            "src/test/expected/api.java.custom.expected");

    private static final String API_JAVA_EMBEDED_EXPECTED = readFileToString(
            "src/test/expected/api.java.embedded.expected");

    private static String MODEL_JAVA_CUSTOM_EXTECTED = readFileToString("src/test/expected/model.java.custom.expected");

    private HandlebarTemplateEngine engine;

    private String templateFile;

    @Spy
    private DefaultCodegenConfig config;

    @Mock
    private Map<String, Object> additionalProperties;

    private Map<String, Object> templateData;

    private static String readFileToString(String filePath) {
        StringBuilder contentBuilder = new StringBuilder();
        try (Stream<String> stream = Files.lines(Paths.get(filePath), StandardCharsets.UTF_8)) {
            stream.forEach(s -> contentBuilder.append(s).append("\n"));
        } catch (IOException e) {
            throw new IllegalStateException(" readFileToString :: ", e);
        }

        contentBuilder.deleteCharAt(contentBuilder.length() - 1);

        return contentBuilder.toString();
    }

    protected String getTemplateDir() {
        return new StringBuilder().append(CodegenConstants.HANDLEBARS_TEMPLATE_ENGINE).append(File.separatorChar)
                .append(getDefaultTemplateDir()).toString();
    }

    private String getDefaultTemplateDir() {
        return "Java";
    }

    @BeforeMethod
    public void setUp() throws IOException {
        MockitoAnnotations.initMocks(this);

//        Files.write(Paths.get("src/test/expected/api.java.custom.expected"), API_JAVA_CUSOM_EXPECTED.getBytes());
//        Files.write(Paths.get("src/test/expected/api.java.embedded.expected"), API_JAVA_EMBEDED_EXPECTED.getBytes());
//        Files.write(Paths.get("src/test/expected/model.java.custom.expected"), MODEL_JAVA_CUSTOM_EXTECTED.getBytes());

        templateFile = "src/test/dir/templates/handlebars/Java/libraries/jersey2/api.mustache";

        engine = new HandlebarTemplateEngine(config);

    }

    @Test
    public void testGetRendered_with_custom_template_dir01_and_exists_in() throws Exception {

        String templateDir = "src/test/dir_01/templates/handlebars/Java";
        when(config.templateDir()).thenReturn(templateDir);
        when(config.getDefaultTemplateDir()).thenReturn("Java");

        //
        when(config.embeddedTemplateDir()).thenReturn(getTemplateDir());

        when(config.additionalProperties()).thenReturn(additionalProperties);
        when(additionalProperties.get(CodegenConstants.TEMPLATE_DIR)).thenReturn(templateDir);

        templateFile = templateDir + "/libraries/jersey2/api.mustache";

        String actual = engine.getRendered(templateFile, templateData);

        assertEquals(actual, API_JAVA_CUSOM_EXPECTED);
    }

    @Test
    public void testGetRendered_with_custom_template_dir01_and_exists_in_without_templatedir_prefix() throws Exception {

        String templateDir = "src/test/dir_01/templates/handlebars/Java";
        when(config.templateDir()).thenReturn(templateDir);
        when(config.getDefaultTemplateDir()).thenReturn("Java");

        //
        when(config.embeddedTemplateDir()).thenReturn(getTemplateDir());

        when(config.additionalProperties()).thenReturn(additionalProperties);
        when(additionalProperties.get(CodegenConstants.TEMPLATE_DIR)).thenReturn(templateDir);

        templateFile = "/libraries/jersey2/api.mustache";

        String actual = engine.getRendered(templateFile, templateData);

        assertEquals(actual, API_JAVA_CUSOM_EXPECTED);
    }
    
    
    
    @Test
    public void testGetRendered_with_custom_template_dir02_and_not_exists() throws Exception {

        String templateDir = "src/test/dir_02/templates/handlebars/Java";
        when(config.templateDir()).thenReturn(templateDir);
        when(config.getDefaultTemplateDir()).thenReturn("Java");
        when(config.embeddedTemplateDir()).thenReturn(getTemplateDir());
        when(config.additionalProperties()).thenReturn(additionalProperties);
        when(additionalProperties.get(CodegenConstants.TEMPLATE_DIR)).thenReturn(templateDir);

        templateFile = config.embeddedTemplateDir() + "/libraries/jersey2/api.mustache";

        String actual = engine.getRendered(templateFile, templateData);

        assertEquals(actual, API_JAVA_EMBEDED_EXPECTED);
    }

    @Test
    public void testGetRendered_without_custom_template_dir() throws Exception {

        String templateDir = "";// "src/test/dir/templates/handlebars/Java";
        when(config.templateDir()).thenReturn(templateDir);
        when(config.getDefaultTemplateDir()).thenReturn("Java");
        when(config.embeddedTemplateDir()).thenReturn(getTemplateDir());
        when(config.additionalProperties()).thenReturn(additionalProperties);
        when(additionalProperties.get(CodegenConstants.TEMPLATE_DIR)).thenReturn(null);

        templateFile = config.embeddedTemplateDir() + "/libraries/jersey2/api.mustache";

        String actual = engine.getRendered(templateFile, templateData);

        assertEquals(actual, API_JAVA_EMBEDED_EXPECTED);
    }

    @Test
    public void testGetRendered_without_custom_template_dir_without_templatedir_prefix() throws Exception {

        String templateDir = "";// "src/test/dir/templates/handlebars/Java";
        when(config.templateDir()).thenReturn(templateDir);
        when(config.getDefaultTemplateDir()).thenReturn("Java");
        when(config.embeddedTemplateDir()).thenReturn(getTemplateDir());
        when(config.additionalProperties()).thenReturn(additionalProperties);
        when(additionalProperties.get(CodegenConstants.TEMPLATE_DIR)).thenReturn(null);

        templateFile = "/libraries/jersey2/api.mustache";

        String actual = engine.getRendered(templateFile, templateData);

        assertEquals(actual, API_JAVA_EMBEDED_EXPECTED);
    }

    @Test
    public void testGetRendered_with_custom_template_dir_file_with_partial() throws Exception {

        String templateDir = "src/test/dir_01/templates/handlebars/Java";
        when(config.templateDir()).thenReturn(templateDir);
        when(config.getDefaultTemplateDir()).thenReturn("Java");
        when(config.embeddedTemplateDir()).thenReturn(getTemplateDir());
        when(config.additionalProperties()).thenReturn(additionalProperties);
        when(additionalProperties.get(CodegenConstants.TEMPLATE_DIR)).thenReturn(templateDir);

        // templateFile = config.embeddedTemplateDir()+ "/model.mustache";

        templateFile = templateDir + "/model.mustache";

        System.out.println(templateFile);
        LOGGER.info(templateFile);

        String actual = engine.getRendered(templateFile, templateData);

        assertEquals(actual, MODEL_JAVA_CUSTOM_EXTECTED);

    }

}
