package io.swagger.codegen.cpprest;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.codegen.languages.CppRestClientCodegen;
import io.swagger.codegen.languages.Swift4Codegen;
import io.swagger.models.Model;

import com.google.common.collect.Sets;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.*;
import org.testng.Assert;
import org.testng.annotations.Test;

public class CppRestClientCodegenTest {

    @Test(description = "default values")
    public void defaultValuesTest() {
        BooleanProperty b = new BooleanProperty();
        b.setDefault(true);

        IntegerProperty i = new IntegerProperty();
        i.setDefault(-42);

        LongProperty l = new LongProperty();
        l.setDefault(-42L);

        FloatProperty f = new FloatProperty();
        f.setDefault(-42f);

        DoubleProperty d = new DoubleProperty();
        d.setDefault(-42.);

        StringProperty s = new StringProperty();
        s.setDefault("unset");

        final Model model = new ModelImpl()
                .description("a sample model")
                .property("b", b).property("bf", new BooleanProperty())
                .property("i", i).property("if", new IntegerProperty())
                .property("l", l).property("lf", new LongProperty())
                .property("f", f).property("ff", new FloatProperty())
                .property("d", d).property("df", new DoubleProperty())
                .property("s", s).property("sf", new StringProperty());
        final DefaultCodegen cg = new CppRestClientCodegen();
        final CodegenModel cm = cg.fromModel("sample", model);

        int idx=0;
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "b");
            Assert.assertEquals(property.datatype, "bool");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "true");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "bf");
            Assert.assertEquals(property.datatype, "bool");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "false");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "i");
            Assert.assertEquals(property.datatype, "int32_t");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "-42");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "if");
            Assert.assertEquals(property.datatype, "int32_t");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "0");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "l");
            Assert.assertEquals(property.datatype, "int64_t");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "-42L");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "lf");
            Assert.assertEquals(property.datatype, "int64_t");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "0L");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "f");
            Assert.assertEquals(property.datatype, "float");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "-42.0f");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "ff");
            Assert.assertEquals(property.datatype, "float");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "0.0f");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "d");
            Assert.assertEquals(property.datatype, "double");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "-42.0");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "df");
            Assert.assertEquals(property.datatype, "double");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "0.0");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "s");
            Assert.assertEquals(property.datatype, "utility::string_t");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "utility::conversions::to_string_t(\"unset\")");
        }
        {
            final CodegenProperty property = cm.vars.get(idx++);
            Assert.assertEquals(property.baseName, "sf");
            Assert.assertEquals(property.datatype, "utility::string_t");
            Assert.assertNotNull(property.defaultValue);
            Assert.assertEquals(property.defaultValue, "utility::conversions::to_string_t(\"\")");
        }
    }
}