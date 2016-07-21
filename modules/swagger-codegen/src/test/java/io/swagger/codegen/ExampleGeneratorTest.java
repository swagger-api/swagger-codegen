package io.swagger.codegen;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import io.swagger.codegen.examples.ExampleGenerator;
import io.swagger.models.*;
import io.swagger.models.properties.*;
import io.swagger.util.Json;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.*;

@SuppressWarnings("static-method")
public class ExampleGeneratorTest {

    private static final String JSON = "application/json";
    private static final String XML = "application/xml";

    @Test(description = "check handling of recursive models")
    public void recursiveModelsTest() {
        final String nodeType = "Node";
        final RefProperty ref = new RefProperty(nodeType);
        final Model node = new ModelImpl().name(nodeType).property("name", new StringProperty())
                .property("parent", ref)
                .property("children", new ArrayProperty(ref))
                .property("wrappedChildren", new ArrayProperty(ref).xml(new Xml().wrapped(true)));
        final String pairType = "Pair";
        final ModelImpl pair = new ModelImpl().name(pairType);
        for (Map.Entry<String, String> item : ImmutableMap.of("first", "First", "second", "Second").entrySet()) {
            final RefProperty property = new RefProperty(nodeType);
            property.setXml(new Xml().name(item.getValue()));
            pair.property(item.getKey(), property);

        }
        final Set<String> types = Sets.newHashSet();
        final List<String> expectedTypes = Arrays.asList(JSON, XML);

        final ExampleGenerator eg = new ExampleGenerator(ImmutableMap.of(nodeType, node, pairType, pair));
        for (Map<String, String> item : eg.generate(null, expectedTypes, new RefProperty(pairType))) {
            final String example = item.get("example");
            final String contentType = item.get("contentType");
            if (XML.equals(contentType)) {
                types.add(XML);
                Assert.assertEquals(example, "<Pair>\n" +
                        "  <Node>\n" +
                        "    <name>string</name>\n" +
                        "    <wrappedChildren>\n" +
                        "    </wrappedChildren>\n" +
                        "  </Node>\n" +
                        "  <Node>\n" +
                        "    <name>string</name>\n" +
                        "    <wrappedChildren>\n" +
                        "    </wrappedChildren>\n" +
                        "  </Node>\n" +
                        "</Pair>");
            } else if (JSON.equals(contentType)) {
                types.add(JSON);
                // TODO - add JSON validation
                Assert.assertNotNull(example);
            }
        }

        Assert.assertEqualsNoOrder(types.toArray(new String[types.size()]),
                expectedTypes.toArray(new String[expectedTypes.size()]));
    }

    @Test(description = "check primitive types generation")
    public void testPrimitiveTypes() throws Exception {
        final String nodeType = "Node";
        RefProperty ref = new RefProperty(nodeType);
        Model node = new ModelImpl().name(nodeType)
                .property("parent", ref)
                .property("str", new StringProperty())
                .property("bool", new BooleanProperty())
                .property("dbl", new DoubleProperty())
                .property("float", new FloatProperty())
                .property("int", new IntegerProperty())
                .property("long", new LongProperty())
                .property("obj", new ObjectProperty());

        ExampleGenerator generator = new ExampleGenerator(ImmutableMap.of(nodeType, node));
        List<Map<String, String>> generate = generator.generate(null, Collections.singletonList(JSON), ref);
        String example = generate.get(0).get("example");

        Map<String, Object> result = Json.mapper().readValue(example, new TypeReference<Map<String, Object>>() {});
        Assert.assertEquals(result.get("str"), "aeiou");
        Assert.assertEquals(result.get("bool"), Boolean.TRUE);
        Assert.assertEquals(result.get("dbl"), 3.149);
        Assert.assertEquals(result.get("float"), 1.23);
        Assert.assertEquals(result.get("int"), 123);
        Assert.assertEquals(result.get("long"), 123456789);
        Assert.assertTrue(result.get("obj") instanceof Map);
        Assert.assertEquals(((Map)result.get("obj")).size(), 0);
    }

    @Test(description = "check composed model generation")
    public void testComposedModel() throws Exception {
        final String nodeType = "Node";
        RefProperty ref = new RefProperty(nodeType);
        RefProperty parent = new RefProperty("parent");
        Model modelOne = new ModelImpl().name("ModelOne")
                .property("parent", parent)
                .property("str", new StringProperty())
                .property("bool", new BooleanProperty())
                .property("dbl", new DoubleProperty());

        Model modelTwo = new ModelImpl().name("ModelTwo")
                .property("parent", parent)
                .property("float", new FloatProperty())
                .property("int", new IntegerProperty())
                .property("long", new LongProperty())
                .property("obj", new ObjectProperty());

        Model model = new ComposedModel()
                .interfaces(Arrays.asList(new RefModel("ModelOne"), new RefModel("ModelTwo")))
                .parent(new RefModel(nodeType));

        ExampleGenerator generator = new ExampleGenerator(ImmutableMap.of("ModelOne", modelOne, "ModelTwo", modelTwo, nodeType, model));
        List<Map<String, String>> generate = generator.generate(null, Collections.singletonList(JSON), ref);
        String example = generate.get(0).get("example");

        Map<String, Object> result = Json.mapper().readValue(example, new TypeReference<Map<String, Object>>() {});
        Assert.assertEquals(result.get("str"), "aeiou");
        Assert.assertEquals(result.get("bool"), Boolean.TRUE);
        Assert.assertEquals(result.get("dbl"), 3.149);
        Assert.assertEquals(result.get("float"), 1.23);
        Assert.assertEquals(result.get("int"), 123);
        Assert.assertEquals(result.get("long"), 123456789);
        Assert.assertTrue(result.get("obj") instanceof Map);
        Assert.assertEquals(((Map)result.get("obj")).size(), 0);
    }
}
