package io.swagger.codegen.csharp;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.properties.StringProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class CSharpInheritanceTest {

    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with parent")
    public void javaInheritanceTest() {
        final Model model = new ComposedModel().parent(new RefModel("ModelBase"))
                .child(new ModelImpl().additionalProperties(new StringProperty()));

        final DefaultCodegen codegen = new CSharpClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "ModelBase");
        Assert.assertEquals(cm.imports, Sets.newHashSet("ModelBase"));
    }

    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with discriminator")
    public void javaInheritanceWithDiscriminatorTest() {
        ModelImpl base = new ModelImpl();
        base.setDiscriminator("disc");

        final Model model = new ComposedModel()
                .interfaces(Arrays.asList(new RefModel("ModelBase")))
                .child(new ModelImpl().additionalProperties(new StringProperty()));

        final Map<String, Model> allDefinitions = new HashMap<String, Model>();
        allDefinitions.put("ModelBase", base);

        final DefaultCodegen codegen = new CSharpClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, allDefinitions);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "ModelBase");
        Assert.assertEquals(cm.imports, Sets.newHashSet("ModelBase"));
    }
}
