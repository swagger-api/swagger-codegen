package io.swagger.codegen.jaxrs;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.JavaResteasyEapServerCodegen;
import io.swagger.codegen.languages.JavaResteasyServerCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;

import static org.testng.Assert.assertTrue;

public class JavaResteasyOuterEnumTest {

    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeMethod
    public void setUp() throws Exception {
        folder.create();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        folder.delete();
    }

    @DataProvider(name = "codegenConfigs")
    public Object[][] codegenConfigs() {
        return new Object[][]{
                {new JavaResteasyServerCodegen()},
                {new JavaResteasyEapServerCodegen()}
        };
    }

    @Test(dataProvider = "codegenConfigs", description = "outer enum has value constructor and @JsonValue")
    public void outerEnumTest(CodegenConfig codegenConfig) throws IOException {
        final File outputFolder = folder.getRoot();
        final Swagger swagger = new SwaggerParser().read("2_0/issue-3856.yaml");
        codegenConfig.setOutputDir(outputFolder.getAbsolutePath());

        final ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);
        new DefaultGenerator().opts(clientOptInput).generate();

        final File outerEnum = new File(outputFolder, "src/gen/java/io/swagger/model/OrderStatus.java");
        assertTrue(outerEnum.exists());
        assertTrue(containsString(outerEnum, "import com.fasterxml.jackson.annotation.JsonValue;"));
        assertTrue(containsString(outerEnum, "public enum OrderStatus {"));
        assertTrue(containsString(outerEnum, "PLACED(\"placed\"),"));
        assertTrue(containsString(outerEnum, "APPROVED(\"approved\"),"));
        assertTrue(containsString(outerEnum, "DELIVERED(\"delivered\");"));
        assertTrue(containsString(outerEnum, "private String value;"));
        assertTrue(containsString(outerEnum, "OrderStatus(String value) {"));
        assertTrue(containsString(outerEnum, "@JsonValue\n  public String toString() {"));
    }

    private boolean containsString(File file, String search) throws IOException {
        return normalize(FileUtils.readFileToString(file)).contains(normalize(search));
    }

    private String normalize(String value) {
        return value.replace("\r\n", "\n");
    }
}
