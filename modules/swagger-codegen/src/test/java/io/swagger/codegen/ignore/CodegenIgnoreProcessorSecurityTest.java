package io.swagger.codegen.ignore;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;

public class CodegenIgnoreProcessorSecurityTest {

    @Test
    public void testConstructorWithPathTraversal() {
        Assert.assertThrows(
                "shouldOverwrite should throw SecurityException for suspicious path",
                SecurityException.class,
                () -> new CodegenIgnoreProcessor("../../../etc")
        );
    }

    @Test
    public void testFileConstructorWithPathTraversal() {
        File maliciousFile = new File("../../../etc/passwd");

        Assert.assertThrows(
                "shouldOverwrite should throw SecurityException for suspicious path",
                SecurityException.class,
                () -> new CodegenIgnoreProcessor(maliciousFile)
        );
    }
}
