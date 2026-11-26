//
// Unit tests for ExampleGenerator.
//

package io.swagger.codegen.v3.examples;

import io.swagger.v3.core.converter.ModelConverters;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Size;
import org.testng.Assert;
import org.testng.annotations.Test;

public class ExampleGeneratorTest {

  @Test
  public void testGenerateJson() throws IOException {
    // Read expected JSON from a resource file.
    String expectedJson = readResource("examples/example_generator.json");

    // Read the models.
    Map<String, io.swagger.v3.oas.models.media.Schema> mc =
        ModelConverters.getInstance().readAll(Example.class);
    Set<String> expectedKeys = new HashSet<>();
    Collections.addAll(expectedKeys, "Example", "ExampleItem", "ExampleCoordinate");
    Assert.assertEquals(mc.keySet(), expectedKeys);

    // Pass the models into the ExampleGenerator and generate the example JSON.
    ExampleGenerator generator = new ExampleGenerator(mc);
    List<Map<String, String>> examples =
        generator.generate(null, Collections.singletonList("application/json"), "Example");
    String example = examples.get(0).get("example");

    Assert.assertEquals(example, expectedJson);

    // Read the example JSON using the Json mapper to ensure that the generated JSON is
    // deserializable back to an object.
    Example exampleInstance = Json.mapper().readValue(example, Example.class);
    Assert.assertNotNull(exampleInstance);
  }

  //
  // Read a resource file into a new String.
  //
  // The file contents must be encoded in UTF-8.
  //
  private String readResource(String name) throws IOException {
    InputStream stream = getClass().getClassLoader().getResourceAsStream(name);
    byte[] data = new byte[10000];
    int length = Objects.requireNonNull(stream).read(data);
    return new String(data, 0, length, StandardCharsets.UTF_8);
  }

  // Example classes used by the above test cases.
  static class Example {
    public Integer intNoAnnotation;

    @Schema(description = "An integer", defaultValue = "99", example = "440")
    public int intDefaultExample;

    @Schema(description = "An integer", defaultValue = "99")
    public Integer intDefault;

    @Min(1500)
    @Max(1550)
    public int intMinMax;

    public String strNoAnnotation;

    @Schema(description = "A string", defaultValue = "computer", example = ":-D")
    public String strDefaultExample;

    @Schema(description = "A string", defaultValue = "computer")
    public String strDefault;

    @Schema(
        description = "A string",
        allowableValues = {"mercury", "venus", "mars"})
    public String strAllowableValues;

    @Schema(description = "A string", format = "url")
    public String strFormatUrl;

    @Schema(description = "A string", format = "uuid")
    public String strFormatUuid;

    @Size(min = 21, max = 31)
    public String strSizeMinMax;

    public boolean boolNoAnnotation;

    @Schema(description = "A boolean", defaultValue = "true", example = "false")
    public Boolean boolDefaultExample;

    @Schema(description = "A boolean", defaultValue = "false")
    public Boolean boolDefault;

    public BigDecimal decimalNoAnnotation;

    @Schema(description = "A decimal", defaultValue = "77.77", example = "7700.0077")
    public BigDecimal decimalDefaultExample;

    @Schema(description = "A decimal", defaultValue = "77.77")
    public BigDecimal decimalDefault;

    @DecimalMin("21.000")
    @DecimalMax("23.999")
    public BigDecimal decimalMinMax;

    public List<String> arrayNoAnnotation;

    @ArraySchema(
        arraySchema =
            @Schema(
                description = "An array of strings",
                example = "[\"AAA111\", \"BBB222\", \"CCC333\"]"))
    public List<String> arrayExample;

    public List<ExampleItem> arrayItemNoAnnotation;

    @Size(max = 20)
    public List<ExampleItem> arrayItemSizeMax;

    @ArraySchema(
        arraySchema =
            @Schema(
                description = "An array of items",
                example =
                    "[{"
                        + "  \"str\": \"alpha\","
                        + "  \"decimal\": 6.01,"
                        + "  \"coordinate\": {\"x\": -4, \"y\": 12, \"z\": 16 }"
                        + "}, {"
                        + "  \"str\": \"beta\","
                        + "  \"decimal\": -3.005,"
                        + "  \"coordinate\": {\"x\": 3, \"y\": 7, \"z\": 21 }"
                        + "}]"))
    public List<ExampleItem> arrayItemExample;

    public LocalDate dateNoAnnotation;

    public LocalDateTime dateTimeNoAnnotation;
  }

  static class ExampleItem {
    public String str;
    public BigDecimal decimal;
    public ExampleCoordinate coordinate;
  }

  static class ExampleCoordinate {
    public int x;
    public int y;
    public int z;
  }
}
