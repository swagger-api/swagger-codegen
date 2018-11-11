package io.swagger.codegen.java;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.codegen.languages.SpringCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.lang3.StringUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class JavaGeneratorTest {

    public TemporaryFolder folder = new TemporaryFolder();

    private static final String MODEL_ORDER_FILE = "/src/main/java/io/swagger/client/model/Order.java";

    @BeforeMethod
    public void setUp() throws Exception {
        folder.create();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        folder.delete();
    }

    @Test
    public void testApiModelAnnontationsAreSuppressedForJava() throws IOException {
        CodegenConfig cc = new JavaClientCodegen();
        cc.setLibrary("jersey2");
        testGenerate(true, cc);
    }

    @Test
    public void testApiModelAnnonationsAreEnabledForJava() throws IOException {
        CodegenConfig cc = new JavaClientCodegen();
        cc.setLibrary("jersey2");
        testGenerate(false, cc);
    }

    @Test
    public void testApiModelAnnontationsAreSuppressedForSpring() throws IOException {
        CodegenConfig cc = new SpringCodegen();
        cc.setLibrary("spring-mvc");
        testGenerate(true, cc);
    }

    @Test
    public void testApiModelAnnonationsAreEnabledForSpring() throws IOException {
        CodegenConfig cc = new SpringCodegen();
        cc.setLibrary("spring-mvc");
        testGenerate(false, cc);
    }

    private void testGenerate(boolean disabled, CodegenConfig codegenConfig) throws IOException {
        final File output = folder.getRoot();

        final Swagger swagger = new SwaggerParser().read("src/test/resources/petstore.json");
        codegenConfig.setOutputDir(output.getAbsolutePath());
        ClientOpts clientOpts = new ClientOpts();
        clientOpts.setProperties(configProperties(disabled));
        ClientOptInput opts = new ClientOptInput()
                .config(codegenConfig)
                .opts(clientOpts)
                .swagger(swagger);

        ClientOptInput clientOptInput = new ClientOptInput().opts(clientOpts).swagger(swagger).config(codegenConfig);
        new DefaultGenerator().opts(clientOptInput).generate();

        final File order = new File(output, MODEL_ORDER_FILE);
        assertTrue(order.exists());

        if (disabled)
            assertFalse(containsAnnotationsFor(order, "ApiModel", "ApiModelProperty"));
        else
            assertTrue(containsAnnotationsFor(order, "ApiModel", "ApiModelProperty"));
    }

    private Map<String, String> configProperties(boolean disabled) {
        Map<String, String> props = new HashMap<>();
        props.put("disableApiModelAnnotations", String.valueOf(disabled));
        props.put("modelPackage", "io.swagger.client.model");
        return props;
    }

    private boolean containsAnnotationsFor(File file, String ...search) throws IOException {
        for (String line : Files.readAllLines(file.toPath(), Charset.defaultCharset())) {
            for (String term : search ) {
                if (StringUtils.indexOf(line, "@" + term) > 0) {
                    return true;
                }
            }
        }
        return false;
    }
}
