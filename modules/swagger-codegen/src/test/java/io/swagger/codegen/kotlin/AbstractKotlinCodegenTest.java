package io.swagger.codegen.kotlin;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.java.AbstractJavaCodegenTest;
import io.swagger.codegen.languages.AbstractJavaCodegen;
import io.swagger.codegen.languages.AbstractKotlinCodegen;
import io.swagger.codegen.languages.KotlinClientCodegen;
import io.swagger.codegen.languages.KotlinServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import static io.swagger.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE;
import static io.swagger.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase;
import static io.swagger.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original;
import static io.swagger.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase;
import static io.swagger.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case;

public class AbstractKotlinCodegenTest {

    private final AbstractKotlinCodegen codegen = new P_AbstractKotlinCodegen();

    @Test
    public void camlCaseEnumConverter() {
        codegen.setEnumPropertyNaming(camelCase.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "longName");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1longName");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "not1longName");
    }

    @Test
    public void uppercasEnumConverter() {
        codegen.setEnumPropertyNaming(UPPERCASE.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "LONG_NAME");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1LONG_NAME");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "NOT1LONG_NAME");
    }
    @Test
    public void snake_caseEnumConverter() {
        codegen.setEnumPropertyNaming(snake_case.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "long_name");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1long_name");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "not1long_name");
    }

    @Test
    public void originalEnumConverter() {
        codegen.setEnumPropertyNaming(original.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "long_Name");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1long_Name");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "not1long_Name");
    }
    @Test
    public void pascalCaseEnumConverter() {
        codegen.setEnumPropertyNaming(PascalCase.name());
        Assert.assertEquals(codegen.toEnumVarName("long Name", null), "LongName");
        Assert.assertEquals(codegen.toEnumVarName("1long Name", null), "_1longName");
        Assert.assertEquals(codegen.toEnumVarName("not1long Name", null), "Not1longName");
    }


    private class P_AbstractKotlinCodegen extends AbstractKotlinCodegen {
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
    }
}
