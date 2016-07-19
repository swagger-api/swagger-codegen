package io.swagger.codegen.languages;

import org.testng.Assert;
import org.testng.annotations.Test;

public class AbstractJavaCodegenTest {

    @Test
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        AbstractJavaCodegen javaCodegen = new FakeJavaCodegen();
        Assert.assertEquals("_", javaCodegen.toEnumVarName("_", "String"));
        Assert.assertEquals("__", javaCodegen.toEnumVarName("__", "String"));
        Assert.assertEquals("__", javaCodegen.toEnumVarName("_,.", "String"));
    }

}