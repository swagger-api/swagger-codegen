package io.swagger.codegen.go;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.GoClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import io.swagger.parser.util.ParseOptions;
import org.junit.rules.TemporaryFolder;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.reporters.Files;

import java.io.File;
import java.io.IOException;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class GoClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final GoClientCodegen codegen = new GoClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    public static class Issue11775Test {

        private static final String EXPECTED_MODEL = "package swagger\n" +
                "\n" +
                "import (\n" +
                "\t\"github.com/google/uuid\"\n" +
                "\t\"time\"\n" +
                ")\n" +
                "\n" +
                "type PetDto struct {\n" +
                "\t// The creation timestamp\n" +
                "\tCreatedAt time.Time `json:\"createdAt,omitempty\"`\n" +
                "\t// The user who created\n" +
                "\tCreatedBy *uuid.UUID `json:\"createdBy,omitempty\"`\n" +
                "\t// The ID\n" +
                "\tId *uuid.UUID `json:\"id,omitempty\"`\n" +
                "\t// The name of the pet\n" +
                "\tName string `json:\"name,omitempty\"`\n" +
                "}";

        private static final String EXPECTED_MODEL_WITH_NESTED_IMPORTS = "import (\n" +
                "import (\n" +
                "\t\"github.com/google/uuid\"\n" +
                "\t\"time\"\n" +
                ")\n" +
                "\n" +
                ")";

        private static final TemporaryFolder TMP_FOLDER = new TemporaryFolder();

        private CodegenConfig codegenConfig;
        private Swagger swagger;

        @BeforeClass
        public void init() {
            final ParseOptions parseOptions = new ParseOptions();
            parseOptions.setFlatten(true);

            swagger = new SwaggerParser().read("2_0/go/petWithGoogleUuid.json", null, parseOptions);

            codegenConfig = new GoClientCodegen();
            codegenConfig.typeMapping().put("UUID", "uuid.UUID");
            codegenConfig.importMapping().put("uuid.UUID", "github.com/google/uuid");
        }

        @BeforeMethod
        public void setUp() throws Exception {
            TMP_FOLDER.create();
        }

        @AfterMethod
        public void tearDown() {
            TMP_FOLDER.delete();
        }

        @Test(description = "generate model with many imports")
        public void testIssue11775() throws Exception {
            codegenConfig.setOutputDir(TMP_FOLDER.getRoot().getAbsolutePath());

            final ClientOptInput clientOptInput = new ClientOptInput()
                    .opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

            new DefaultGenerator().opts(clientOptInput).generate();

            final File model = new File(TMP_FOLDER.getRoot(), "model_pet_dto.go");
            assertTrue(model.exists());
            assertTrue(containsString(model, EXPECTED_MODEL));
        }

        @Test(description = "generate invalid model caused by nested imports")
        public void testIssue11775WithNestedImport() throws Exception {
            codegenConfig.setOutputDir(TMP_FOLDER.getRoot().getAbsolutePath());

            final ClientOptInput clientOptInput = new ClientOptInput()
                    .opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

            new DefaultGenerator().opts(clientOptInput).generate();

            final File model = new File(TMP_FOLDER.getRoot(), "model_pet_dto.go");
            assertTrue(model.exists());
            assertFalse(containsString(model, EXPECTED_MODEL_WITH_NESTED_IMPORTS));
        }

        private static boolean containsString(final File file, final String text) throws IOException {
            return trim(Files.readFile(file)).contains(trim(text));
        }

        private static String trim(final String value) {
            return value.replaceAll("[ \n\r\t]", "");
        }

    }

}
