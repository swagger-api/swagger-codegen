package io.swagger;

import org.junit.Test;

import java.io.IOException;

public class Jaxb2SchemaTest {

  public static final String FIXTURE = "schema.xsd";
  public static final String SCHEMA = "./target/generated-resources/schemagen/schema1.xsd";

  @Test
  public void generatedSchemasMustBeSame() throws IOException {

    Assertions.assertFileEquals(FIXTURE, SCHEMA);
  }
}
