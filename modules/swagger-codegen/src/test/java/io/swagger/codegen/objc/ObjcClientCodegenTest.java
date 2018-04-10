package io.swagger.codegen.objc;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.ObjcClientCodegen;

public class ObjcClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final ObjcClientCodegen codegen = new ObjcClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), "true");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final ObjcClientCodegen codegen = new ObjcClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
    }

}
