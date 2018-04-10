package io.swagger.codegen.rust;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.RustClientCodegen;

public class RustClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
    }

}
