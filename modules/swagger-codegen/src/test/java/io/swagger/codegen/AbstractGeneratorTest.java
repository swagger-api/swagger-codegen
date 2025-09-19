package io.swagger.codegen;

import org.testng.annotations.Test;

import java.io.IOException;

public class AbstractGeneratorTest {


    private static class TestableAbstractGenerator extends AbstractGenerator {
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testWriteToFileWithPathTraversal() throws IOException {
        TestableAbstractGenerator generator = new TestableAbstractGenerator();
        generator.writeToFile("../../../etc/passwd", "malicious content");
    }
}
