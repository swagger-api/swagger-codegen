package io.swagger.codegen;

import io.swagger.models.properties.AbstractProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.IntegerProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

public class DefaultCodegenTest {

    /**
     * A test property class that simulates the behavior of properties returned by the
     * swagger-parser when parsing OpenAPI 3.1.0 schemas where the type might be null.
     */
    private static class NullTypeProperty extends AbstractProperty {
        private String format;

        public NullTypeProperty() {
            super();
            // Set type to null to simulate OpenAPI 3.1.0 parsing issue
            super.setType(null);
        }

        public NullTypeProperty(String format) {
            this();
            this.format = format;
            super.setFormat(format);
        }

        @Override
        public String getType() {
            return null;
        }
    }

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

    @Test
    public void testShouldOverwriteWithPathTraversal() {
        DefaultCodegen codegen = new DefaultCodegen();
        Assert.assertThrows(
            "shouldOverwrite should throw SecurityException for suspicious path",
            SecurityException.class,
            () -> codegen.shouldOverwrite("../../../etc/passwd")
        );
    }

    /**
     * Test that getSwaggerType returns proper types for standard property types.
     */
    @Test
    public void testGetSwaggerTypeForStandardProperties() {
        final DefaultCodegen codegen = new DefaultCodegen();

        // Test StringProperty
        Assert.assertEquals(codegen.getSwaggerType(new StringProperty()), "string");

        // Test BooleanProperty
        Assert.assertEquals(codegen.getSwaggerType(new BooleanProperty()), "boolean");

        // Test IntegerProperty
        Assert.assertEquals(codegen.getSwaggerType(new IntegerProperty()), "integer");
    }

    /**
     * Test that getSwaggerType handles null property type gracefully by defaulting to "object".
     * This is important for OpenAPI 3.1.0 schemas where type information may be null.
     * See GitHub issue #12666.
     */
    @Test
    public void testGetSwaggerTypeForNullTypeProperty() {
        final DefaultCodegen codegen = new DefaultCodegen();

        // Test property with null type and no format
        Property nullTypeProp = new NullTypeProperty();
        String result = codegen.getSwaggerType(nullTypeProp);
        Assert.assertNotNull(result, "getSwaggerType should never return null");
        Assert.assertEquals(result, "object", "Null type without format should default to 'object'");
    }

    /**
     * Test that getSwaggerType can infer type from format when type is null.
     * This is particularly useful for OpenAPI 3.1.0 schemas.
     * See GitHub issue #12666.
     */
    @Test
    public void testGetSwaggerTypeInfersTypeFromFormat() {
        final DefaultCodegen codegen = new DefaultCodegen();

        // Test format int32 -> integer
        Property int32Prop = new NullTypeProperty("int32");
        Assert.assertEquals(codegen.getSwaggerType(int32Prop), "integer");

        // Test format int64 -> long
        Property int64Prop = new NullTypeProperty("int64");
        Assert.assertEquals(codegen.getSwaggerType(int64Prop), "long");

        // Test format float -> float
        Property floatProp = new NullTypeProperty("float");
        Assert.assertEquals(codegen.getSwaggerType(floatProp), "float");

        // Test format double -> double
        Property doubleProp = new NullTypeProperty("double");
        Assert.assertEquals(codegen.getSwaggerType(doubleProp), "double");

        // Test format date -> date
        Property dateProp = new NullTypeProperty("date");
        Assert.assertEquals(codegen.getSwaggerType(dateProp), "date");

        // Test format date-time -> DateTime
        Property dateTimeProp = new NullTypeProperty("date-time");
        Assert.assertEquals(codegen.getSwaggerType(dateTimeProp), "DateTime");

        // Test format uuid -> UUID
        Property uuidProp = new NullTypeProperty("uuid");
        Assert.assertEquals(codegen.getSwaggerType(uuidProp), "UUID");

        // Test format binary -> binary
        Property binaryProp = new NullTypeProperty("binary");
        Assert.assertEquals(codegen.getSwaggerType(binaryProp), "binary");

        // Test format byte -> ByteArray
        Property byteProp = new NullTypeProperty("byte");
        Assert.assertEquals(codegen.getSwaggerType(byteProp), "ByteArray");

        // Test string-like formats
        Property emailProp = new NullTypeProperty("email");
        Assert.assertEquals(codegen.getSwaggerType(emailProp), "string");

        Property passwordProp = new NullTypeProperty("password");
        Assert.assertEquals(codegen.getSwaggerType(passwordProp), "string");

        Property uriProp = new NullTypeProperty("uri");
        Assert.assertEquals(codegen.getSwaggerType(uriProp), "string");
    }

    /**
     * Test that getSwaggerType falls back to "object" for unknown format when type is null.
     * See GitHub issue #12666.
     */
    @Test
    public void testGetSwaggerTypeFallsBackToObjectForUnknownFormat() {
        final DefaultCodegen codegen = new DefaultCodegen();

        // Test unknown format - should fall back to object
        Property unknownFormatProp = new NullTypeProperty("unknown-format");
        String result = codegen.getSwaggerType(unknownFormatProp);
        Assert.assertNotNull(result, "getSwaggerType should never return null");
        Assert.assertEquals(result, "object", "Unknown format should fall back to 'object'");
    }
}
