package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.BooleanProperty;
import org.junit.Test;
import org.testng.Assert;

public class JavaJAXRSSpecInterfaceCodegenTest {

    private JavaJAXRSSpecInterfaceCodegen generator = new JavaJAXRSSpecInterfaceCodegen();

    @Test
    public void never_process_RestApplication() {
        generator.processOpts();
        for (SupportingFile file : generator.supportingFiles()) {
            Assert.assertNotEquals("RestApplication.mustache", file.templateFile);
        }
    }

    @Test
    public void never_process_pom_from_superclass() {
        generator.processOpts();
        for (SupportingFile file : generator.supportingFiles()) {
            Assert.assertNotEquals("pom.mustache", file.templateFile);
        }
    }

    @Test
    public void do_not_process_pomInterface_by_default() {
        generator.processOpts();
        for (SupportingFile file : generator.supportingFiles()) {
            Assert.assertNotEquals("pomInterface.mustache", file.templateFile);
        }
    }

    @Test
    public void process_pomInterface_if_createPom_is_true() {
        generator.additionalProperties().put("createPom", "true");
        generator.processOpts();
        for (SupportingFile file : generator.supportingFiles()) {
            if ("pomInterface.mustache".equals(file.templateFile)) {
                return;
            }
        }
        Assert.fail("Missing pomInterface.mustache");
    }

    @Test
    public void do_not_process_pomInterface_if_createPom_is_false() {
        generator.additionalProperties().put("createPom", "false");
        generator.processOpts();
        for (SupportingFile file : generator.supportingFiles()) {
            Assert.assertNotEquals("pomInterface.mustache", file.templateFile);
        }
    }

    @Test
    public void verify_that_createPom_exists_as_a_parameter_with_default_false() {
        for (CliOption option : generator.cliOptions()) {
            if (option.getOpt().equals("createPom")) {
                Assert.assertEquals(BooleanProperty.TYPE, option.getType());
                Assert.assertEquals("false", option.getDefault());
                return;
            }
        }
        Assert.fail("Missing createPom");
    }
}
