package io.swagger.codegen.perl;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.PerlClientCodegen;

public class PerlClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PerlClientCodegen codegen = new PerlClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PerlClientCodegen codegen = new PerlClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
    }

}
