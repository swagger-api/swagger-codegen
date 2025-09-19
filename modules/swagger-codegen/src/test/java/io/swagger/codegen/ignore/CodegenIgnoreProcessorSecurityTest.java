package io.swagger.codegen.ignore;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;

public class CodegenIgnoreProcessorSecurityTest {

    @Test
    public void testConstructorWithPathTraversal() {
        CodegenIgnoreProcessor processor = new CodegenIgnoreProcessor("../../../etc");

        Assert.assertNotNull(processor, "Processor should be created despite security violation");
        Assert.assertTrue(processor.getExclusionRules().isEmpty(), "No exclusion rules should be loaded");
        Assert.assertTrue(processor.getInclusionRules().isEmpty(), "No inclusion rules should be loaded");
    }

    @Test
    public void testFileConstructorWithPathTraversal() {
        File maliciousFile = new File("../../../etc/passwd");
        CodegenIgnoreProcessor processor = new CodegenIgnoreProcessor(maliciousFile);

        Assert.assertNotNull(processor, "Processor should be created despite security violation");
        Assert.assertTrue(processor.getExclusionRules().isEmpty(), "No exclusion rules should be loaded");
        Assert.assertTrue(processor.getInclusionRules().isEmpty(), "No inclusion rules should be loaded");
    }
}
