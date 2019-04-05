package io.swagger.codegen;

import io.swagger.models.properties.ComposedProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;

public class DefaultCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false );
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test(description = "datatype for complex ref property")
    public void complexRefPropertyTest() throws Exception {
        final ComposedProperty composed = new ComposedProperty();
        final List<Property> innerProperties = new ArrayList<>();
        innerProperties.add(new RefProperty("#/definitions/Category"));
        composed.setAllOf(innerProperties);

        final DefaultCodegen codegen = new DefaultCodegen();

        String datatype = codegen.getSwaggerType(composed);

        Assert.assertEquals(datatype, "Category");
    }
}
