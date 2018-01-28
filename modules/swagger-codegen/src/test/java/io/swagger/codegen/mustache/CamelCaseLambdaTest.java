package io.swagger.codegen.mustache;

import org.testng.annotations.Factory;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class CamelCaseLambdaTest extends MustacheTestBase {
    private final String input;
    private final String expected;

    public CamelCaseLambdaTest(String input, String expected) {
        this.input = input;
        this.expected = expected;
    }

    @Test(description = "camelCase expected inputs")
    public void testExecute() throws Exception {
        // Arrange
        String template = "{{#camelcase}}{{value}}{{/camelcase}}";
        Object inputCtx = context(
                "camelcase", new CamelCaseLambda(),
                "value", this.input
        );

        // Act
        String actual = compile(template, inputCtx);


        // Assert
        assertEquals(actual, this.expected);
    }

    @Factory
    public static Object[] factoryMethod() {
        return new Object[] {
                new CamelCaseLambdaTest("lowercase input", "lowercase input"),

                // NOTE: DefaultCodegen.camelize(string, true) only results in first character of first word being lowercased.
                // Keeping this behavior as it will match whatever is expected by existing codegen implementations.
                new CamelCaseLambdaTest("UPPERCASE INPUT", "uPPERCASE INPUT"),
                new CamelCaseLambdaTest("inputText", "inputText"),
                new CamelCaseLambdaTest("input_text", "inputText"),

                // TODO: This may be unexpected, but is the result of DefaultCodegen.camelize.
                // CamelCaseLambda can be extended to accept a method reference after move to Java 8.
                new CamelCaseLambdaTest("INPUT_TEXT", "iNPUTTEXT"),
                new CamelCaseLambdaTest("input-text", "inputText"),
                new CamelCaseLambdaTest("input-text input-text input-text input-text input-text", "inputText inputText inputText inputText inputText")
        };
    }
}