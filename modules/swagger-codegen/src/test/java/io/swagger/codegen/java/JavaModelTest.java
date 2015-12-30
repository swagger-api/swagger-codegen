package io.swagger.codegen.java;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.ArrayModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.parameters.QueryParameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.ByteArrayProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;

import com.google.common.collect.Sets;
import io.swagger.util.Json;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class JavaModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .property("createdAt", new DateTimeProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 3);

        final List<CodegenProperty> vars = cm.vars;

        final CodegenProperty property1 = vars.get(0);
        assertEquals(property1.baseName, "id");
        assertEquals(property1.getter, "getId");
        assertEquals(property1.setter, "setId");
        assertEquals(property1.datatype, "Long");
        assertEquals(property1.name, "id");
        assertEquals(property1.defaultValue, "null");
        assertEquals(property1.baseType, "Long");
        assertTrue(property1.hasMore);
        assertTrue(property1.required);
        assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = vars.get(1);
        assertEquals(property2.baseName, "name");
        assertEquals(property2.getter, "getName");
        assertEquals(property2.setter, "setName");
        assertEquals(property2.datatype, "String");
        assertEquals(property2.name, "name");
        assertEquals(property2.defaultValue, "null");
        assertEquals(property2.baseType, "String");
        assertTrue(property2.hasMore);
        assertTrue(property2.required);
        assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = vars.get(2);
        assertEquals(property3.baseName, "createdAt");
        assertEquals(property3.getter, "getCreatedAt");
        assertEquals(property3.setter, "setCreatedAt");
        assertEquals(property3.datatype, "Date");
        assertEquals(property3.name, "createdAt");
        assertEquals(property3.defaultValue, "null");
        assertEquals(property3.baseType, "Date");
        Assert.assertNull(property3.hasMore);
        Assert.assertNull(property3.required);
        assertTrue(property3.isNotContainer);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("urls", new ArrayProperty()
                        .items(new StringProperty()))
                .required("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 2);

        final CodegenProperty property = cm.vars.get(1);
        assertEquals(property.baseName, "urls");
        assertEquals(property.getter, "getUrls");
        assertEquals(property.setter, "setUrls");
        assertEquals(property.datatype, "List<String>");
        assertEquals(property.name, "urls");
        assertEquals(property.defaultValue, "new ArrayList<String>()");
        assertEquals(property.baseType, "List");
        assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("translations", new MapProperty()
                        .additionalProperties(new StringProperty()))
                .required("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "translations");
        assertEquals(property.getter, "getTranslations");
        assertEquals(property.setter, "setTranslations");
        assertEquals(property.datatype, "Map<String, String>");
        assertEquals(property.name, "translations");
        assertEquals(property.defaultValue, "new HashMap<String, String>()");
        assertEquals(property.baseType, "Map");
        assertEquals(property.containerType, "map");
        Assert.assertNull(property.required);
        assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a map with complex list property")
    public void mapWithListPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("translations",
                        new MapProperty().additionalProperties(new ArrayProperty().items(new RefProperty("Pet"))))
                .required("id");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "translations");
        assertEquals(property.getter, "getTranslations");
        assertEquals(property.setter, "setTranslations");
        assertEquals(property.datatype, "Map<String, List<Pet>>");
        assertEquals(property.name, "translations");
        assertEquals(property.defaultValue, "new HashMap<String, List<Pet>>()");
        assertEquals(property.baseType, "Map");
        assertEquals(property.containerType, "map");
        Assert.assertNull(property.required);
        assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with a 2D list property")
    public void list2DPropertyTest() {
        final Model model = new ModelImpl().name("sample").property("list2D", new ArrayProperty().items(
                new ArrayProperty().items(new RefProperty("Pet"))));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "list2D");
        assertEquals(property.getter, "getList2D");
        assertEquals(property.setter, "setList2D");
        assertEquals(property.datatype, "List<List<Pet>>");
        assertEquals(property.name, "list2D");
        assertEquals(property.defaultValue, "new ArrayList<List<Pet>>()");
        assertEquals(property.baseType, "List");
        assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex properties")
    public void complexPropertiesTest() {
        final Model model = new ModelImpl().description("a sample model")
                .property("children", new RefProperty("#/definitions/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "children");
        assertEquals(property.getter, "getChildren");
        assertEquals(property.setter, "setChildren");
        assertEquals(property.datatype, "Children");
        assertEquals(property.name, "children");
        assertEquals(property.defaultValue, "null");
        assertEquals(property.baseType, "Children");
        Assert.assertNull(property.required);
        assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("children", new ArrayProperty().items(new RefProperty("#/definitions/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "children");
        assertEquals(property.complexType, "Children");
        assertEquals(property.getter, "getChildren");
        assertEquals(property.setter, "setChildren");
        assertEquals(property.datatype, "List<Children>");
        assertEquals(property.name, "children");
        assertEquals(property.defaultValue, "new ArrayList<Children>()");
        assertEquals(property.baseType, "List");
        assertEquals(property.containerType, "array");
        Assert.assertNull(property.required);
        assertTrue(property.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("children", new MapProperty().additionalProperties(new RefProperty("#/definitions/Children")));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "a sample model");
        assertEquals(cm.vars.size(), 1);
        assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Map", "List", "Children")).size(), 3);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "children");
        assertEquals(property.complexType, "Children");
        assertEquals(property.getter, "getChildren");
        assertEquals(property.setter, "setChildren");
        assertEquals(property.datatype, "Map<String, Children>");
        assertEquals(property.name, "children");
        assertEquals(property.defaultValue, "new HashMap<String, Children>()");
        assertEquals(property.baseType, "Map");
        assertEquals(property.containerType, "map");
        Assert.assertNull(property.required);
        assertTrue(property.isContainer);
        Assert.assertNull(property.isNotContainer);

    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Model model = new ArrayModel()
                .description("an array model")
                .items(new RefProperty("#/definitions/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "an array model");
        assertEquals(cm.vars.size(), 0);
        assertEquals(cm.parent, "ArrayList<Children>");
        assertEquals(cm.imports.size(), 3);
        assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("List", "ArrayList", "Children")).size(), 3);
    }

    @Test(description = "convert an map model")
    public void mapModelTest() {
        final Model model = new ModelImpl()
                .description("an map model")
                .additionalProperties(new RefProperty("#/definitions/Children"));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.description, "an map model");
        assertEquals(cm.vars.size(), 0);
        assertEquals(cm.parent, "HashMap<String, Children>");
        assertEquals(cm.imports.size(), 3);
        assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Map", "HashMap", "Children")).size(), 3);
    }

    @Test(description = "convert a model with upper-case property names")
    public void upperCaseNamesTest() {
        final Model model = new ModelImpl()
                .description("a model with upper-case property names")
                .property("NAME", new StringProperty())
                .required("NAME");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "NAME");
        assertEquals(property.getter, "getNAME");
        assertEquals(property.setter, "setNAME");
        assertEquals(property.datatype, "String");
        assertEquals(property.name, "NAME");
        assertEquals(property.defaultValue, "null");
        assertEquals(property.baseType, "String");
        Assert.assertNull(property.hasMore);
        assertTrue(property.required);
        assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a model with a 2nd char upper-case property names")
    public void secondCharUpperCaseNamesTest() {
        final Model model = new ModelImpl()
                .description("a model with a 2nd char upper-case property names")
                .property("pId", new StringProperty())
                .required("pId");
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "pId");
        assertEquals(property.getter, "getPId");
        assertEquals(property.setter, "setPId");
        assertEquals(property.datatype, "String");
        assertEquals(property.name, "pId");
        assertEquals(property.defaultValue, "null");
        assertEquals(property.baseType, "String");
        Assert.assertNull(property.hasMore);
        assertTrue(property.required);
        assertTrue(property.isNotContainer);
    }

    @Test(description = "convert hyphens per issue 503")
    public void hyphensTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("created-at", new DateTimeProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "created-at");
        assertEquals(property.getter, "getCreatedAt");
        assertEquals(property.setter, "setCreatedAt");
        assertEquals(property.name, "createdAt");
    }

    @Test(description = "convert query[password] to queryPassword")
    public void squareBracketsTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("query[password]", new StringProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "query[password]");
        assertEquals(property.getter, "getQueryPassword");
        assertEquals(property.setter, "setQueryPassword");
        assertEquals(property.name, "queryPassword");
    }

    @Test(description = "properly escape names per 567")
    public void escapeNamesTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("created-at", new DateTimeProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("with.dots", model);

        assertEquals(cm.classname, "WithDots");
    }

    @Test(description = "convert a model with binary data")
    public void binaryDataTest() {
        final Model model = new ModelImpl()
                .description("model with binary")
                .property("inputBinaryData", new ByteArrayProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "inputBinaryData");
        assertEquals(property.getter, "getInputBinaryData");
        assertEquals(property.setter, "setInputBinaryData");
        assertEquals(property.datatype, "byte[]");
        assertEquals(property.name, "inputBinaryData");
        assertEquals(property.defaultValue, "null");
        assertEquals(property.baseType, "byte[]");
        Assert.assertNull(property.hasMore);
        Assert.assertNull(property.required);
        assertTrue(property.isNotContainer);
    }

    @Test(description = "translate an invalid param name")
    public void invalidParamNameTest() {
        final Model model = new ModelImpl()
                .description("a model with a 2nd char upper-case property names")
                .property("_", new StringProperty());
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.name, "sample");
        assertEquals(cm.classname, "Sample");
        assertEquals(cm.vars.size(), 1);

        final CodegenProperty property = cm.vars.get(0);
        assertEquals(property.baseName, "_");
        assertEquals(property.getter, "getU");
        assertEquals(property.setter, "setU");
        assertEquals(property.datatype, "String");
        assertEquals(property.name, "u");
        assertEquals(property.defaultValue, "null");
        assertEquals(property.baseType, "String");
        Assert.assertNull(property.hasMore);
        assertTrue(property.isNotContainer);
    }

    @Test(description = "convert a parameter")
    public void convertParameterTest() {
        final QueryParameter parameter = new QueryParameter()
                .property(new IntegerProperty())
                .name("limit")
                .required(true);
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenParameter cm = codegen.fromParameter(parameter, null);

        Assert.assertNull(cm.allowableValues);
    }

    @Test(description = "convert a model with BigDecimal property as String")
    public void bigDecimalPropertyTest() {
        StringProperty bigDecimalProperty = new StringProperty();
        bigDecimalProperty.setFormat("number");
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("bigDecimal", bigDecimalProperty);
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model);

        String out = Json.pretty(cm);
        assertTrue(cm.vars.size() == 1);
        assertEquals("BigDecimal", cm.vars.get(0).datatype);
    }

}
