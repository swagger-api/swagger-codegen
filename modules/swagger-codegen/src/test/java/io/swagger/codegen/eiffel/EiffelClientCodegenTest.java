package io.swagger.codegen.eiffel;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.EiffelClientCodegen;

public class EiffelClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final EiffelClientCodegen codegen = new EiffelClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final EiffelClientCodegen codegen = new EiffelClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
    }

}
