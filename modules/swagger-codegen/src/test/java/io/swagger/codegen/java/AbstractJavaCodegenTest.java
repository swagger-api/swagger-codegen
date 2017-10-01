package io.swagger.codegen.java;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.languages.AbstractJavaCodegen;

public class AbstractJavaCodegenTest {

    private final AbstractJavaCodegen fakeJavaCodegen = new AbstractJavaCodegen() {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    };

    @Test
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        Assert.assertEquals("UNDERSCORE", fakeJavaCodegen.toEnumVarName("_", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("__", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("_,.", "String"));
    }

    @Test
    public void toVarNameShouldAvoidOverloadingGetClassMethod() throws Exception {
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("_class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("__class"));
    }

    @Test
    public void toModelNameShouldUseProvidedMapping() throws Exception {
        fakeJavaCodegen.importMapping().put("json_myclass", "com.test.MyClass");
        Assert.assertEquals("com.test.MyClass", fakeJavaCodegen.toModelName("json_myclass"));
    }

    @Test
    public void toModelNameUsesPascalCase() throws Exception {
        Assert.assertEquals("JsonAnotherclass", fakeJavaCodegen.toModelName("json_anotherclass"));
    }

    @DataProvider(name = "toEnumName")
    public static Object[][] toEnumName() {
        return new Object[][]{
                {"lower", "LowerEnum"},
                {"CAPITALIZED", "CAPITALIZEDEnum"},
                {"s", "SEnum"},
                {"D", "DEnum"},
                {"lowHigh", "LowHighEnum"},
                {"HighLow", "HighLowEnum"},
                {"HIGHLow", "HIGHLowEnum"},
                {"HighLOW", "HighLOWEnum"},
        };
    }

    @Test(dataProvider = "toEnumName")
    public void toEnumName(final String propertyName, final String expected) {
        final CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.name = propertyName;
        Assert.assertEquals(fakeJavaCodegen.toEnumName(codegenProperty), expected);
    }

    @DataProvider(name = "toEnumVarNameAndValue")
    public static Object[][] toEnumVarNameAndValue() {
        return new Object[][]{
                {"", null, "EMPTY", "\"\""},
                {"{", null, "LEFT_CURLY_BRACKET", "\"{\""},
                {">=", null, "GREATER_THAN_OR_EQUAL_TO", "\">=\""},
                {"value", null, "VALUE", "\"value\""},
                {"vAlUE", "irrelevant", "VALUE", "\"vAlUE\""},
                {"Value 1", "", "VALUE_1", "\"Value 1\""},
                {"1", null, "_1", "\"1\""},
                {"1.1", "double", "_1_1", "\"1.1\""},
                {"1", "Integer", "NUMBER_1", "1"},
                {"1", "Long", "NUMBER_1", "1L"},
                {"1", "Float", "NUMBER_1_DOT_0", "1.0f"},
                {"1", "Double", "NUMBER_1_DOT_0", "1.0d"},
                {"1.0", "Float", "NUMBER_1_DOT_0", "1.0f"},
                {"1.0", "Double", "NUMBER_1_DOT_0", "1.0d"},
                {"1.1", "Float", "NUMBER_1_DOT_1", "1.1f"},
                {"1.1", "Double", "NUMBER_1_DOT_1", "1.1d"},
                {"1", "List<Integer>", "NUMBER_1", "1"},
                {"1", "List<Long>", "NUMBER_1", "1L"},
                {"1", "List<Float>", "NUMBER_1_DOT_0", "1.0f"},
                {"1", "List<Double>", "NUMBER_1_DOT_0", "1.0d"},
                {"1.0", "List<Float>", "NUMBER_1_DOT_0", "1.0f"},
                {"1.0", "List<Double>", "NUMBER_1_DOT_0", "1.0d"},
                {"1.1", "List<Float>", "NUMBER_1_DOT_1", "1.1f"},
                {"1.1", "List<Double>", "NUMBER_1_DOT_1", "1.1d"},
        };
    }

    @Test(dataProvider = "toEnumVarNameAndValue")
    public void toEnumVarName(final String enumValue, final String datatype, final String expected, final String ignored) {
        Assert.assertEquals(fakeJavaCodegen.toEnumVarName(enumValue, datatype), expected);
    }

    @Test(dataProvider = "toEnumVarNameAndValue")
    public void toEnumValue(final String enumValue, final String datatype, final String ignored, final String expected) {
        Assert.assertEquals(fakeJavaCodegen.toEnumValue(enumValue, datatype), expected);
    }
}
