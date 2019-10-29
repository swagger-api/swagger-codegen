package io.swagger.codegen.swift5;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.Swift5Codegen;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.junit.experimental.theories.suppliers.TestedOn;
import org.testng.Assert;
import org.testng.annotations.Test;

public class Swift5CodegenTest {

    private Swift5Codegen swiftCodegen = new Swift5Codegen();

    @Test
    public void testCapitalizedReservedWord() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("AS", null), "_as");
    }

    @Test
    public void testReservedWord() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("Public", null), "_public");
    }

    @Test
    public void shouldNotBreakNonReservedWord() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("Error", null), "error");
    }

    @Test
    public void shouldNotBreakCorrectName() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("EntryName", null), "entryName");
    }

    @Test
    public void testSingleWordAllCaps() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("VALUE", null), "value");
    }

    @Test
    public void testSingleWordLowercase() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("value", null), "value");
    }

    @Test
    public void testCapitalsWithUnderscore() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY_NAME", null), "entryName");
    }

    @Test
    public void testCapitalsWithDash() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY-NAME", null), "entryName");
    }

    @Test
    public void testCapitalsWithSpace() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY NAME", null), "entryName");
    }

    @Test
    public void testLowercaseWithUnderscore() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("entry_name", null), "entryName");
    }

    @Test
    public void testStartingWithNumber() {
        Assert.assertEquals(swiftCodegen.toEnumVarName("123EntryName", null), "_123entryName");
        Assert.assertEquals(swiftCodegen.toEnumVarName("123Entry_name", null), "_123entryName");
        Assert.assertEquals(swiftCodegen.toEnumVarName("123EntryName123", null), "_123entryName123");
    }

    @Test(description = "returns Data when response format is binary")
    public void binaryDataTest() {
        final Swagger model = new SwaggerParser().read("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new Swift5Codegen();
        final String path = "/tests/binaryResponse";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "Data");
        Assert.assertEquals(op.bodyParam.dataType, "Data");
        Assert.assertTrue(op.bodyParam.isBinary);
        Assert.assertTrue(op.responses.get(0).isBinary);
    }

    @Test(description = "returns Date when response format is date")
    public void dateTest() {
        final Swagger model = new SwaggerParser().read("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new Swift5Codegen();
        final String path = "/tests/dateResponse";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "Date");
        Assert.assertEquals(op.bodyParam.dataType, "Date");
    }

    @Test
    public void testDefaultPodAuthors() {
        // Given

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(Swift5Codegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, Swift5Codegen.DEFAULT_POD_AUTHORS);
    }

    @Test
    public void testPodAuthors() {
        // Given
        final String swaggerDevs = "Swagger Devs";
        swiftCodegen.additionalProperties().put(Swift5Codegen.POD_AUTHORS, swaggerDevs);

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(Swift5Codegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, swaggerDevs);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final Swift5Codegen codegen = new Swift5Codegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final Swift5Codegen codegen = new Swift5Codegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final Swift5Codegen codegen = new Swift5Codegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }
}
