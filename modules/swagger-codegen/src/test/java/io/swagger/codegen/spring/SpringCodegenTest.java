package io.swagger.codegen.spring;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.SpringCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import io.swagger.parser.util.ParseOptions;
import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class SpringCodegenTest {

    public TemporaryFolder folder = new TemporaryFolder();

    public static final String EQUALS_SUPER = "@Override\n" +
            "  public boolean equals(java.lang.Object o) {\n" +
            "    if (this == o) {\n" +
            "      return true;\n" +
            "    }\n" +
            "    if (o == null || getClass() != o.getClass()) {\n" +
            "      return false;\n" +
            "    }\n" +
            "    return super.equals(o);\n" +
            "  }";

    @BeforeMethod
    public void setUp() throws Exception {
        folder.create();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        folder.delete();
    }

    @Test
    public void testIssue7355() throws Exception {
        final File outputFolder = folder.getRoot();
        final ParseOptions parseOptions = new ParseOptions();
        parseOptions.setFlatten(true);

        final Swagger swagger = new SwaggerParser().read("2_0/issue-7355.yaml", null, parseOptions);
        final CodegenConfig codegenConfig = new SpringCodegen();
        codegenConfig.setLibrary("spring-boot");
        codegenConfig.setOutputDir(outputFolder.getAbsolutePath());

        final ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        //generate
        new DefaultGenerator().opts(clientOptInput).generate();

        // Check simple type
        final File modelWithoutProps = new File(outputFolder, "src/main/java/io/swagger/model/ModelWithoutProps.java");
        assertTrue(modelWithoutProps.exists());
        assertTrue(containsString(modelWithoutProps, EQUALS_SUPER));

        // Check simple type
        final File modelWithProps = new File(outputFolder, "src/main/java/io/swagger/model/ModelWithProps.java");
        assertTrue(modelWithProps.exists());
        assertTrue(containsString(modelWithProps, "@Override\n public boolean equals(java.lang.Object o) {"));
        assertFalse(containsString(modelWithProps, EQUALS_SUPER));
    }

    private boolean containsString(File file, String search) throws IOException {
        return trim(FileUtils.readFileToString(file)).contains(trim(search));
    }

    private String trim(String value) {
        return value.replace(" ", "").replace("\n", "").replace("\r", "");
    }

}
