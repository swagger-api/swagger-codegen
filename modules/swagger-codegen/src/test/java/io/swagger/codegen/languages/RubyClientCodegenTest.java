package io.swagger.codegen.languages;

import org.testng.Assert;
import org.testng.annotations.Test;

public class RubyClientCodegenTest {

    @Test
    public void testShouldOverwriteWithPathTraversal() {
        RubyClientCodegen codegen = new RubyClientCodegen();

        boolean result = codegen.shouldOverwrite("../../../etc/passwd");

        Assert.assertFalse(result, "shouldOverwrite should return false when SecurityException is thrown");
    }
}
