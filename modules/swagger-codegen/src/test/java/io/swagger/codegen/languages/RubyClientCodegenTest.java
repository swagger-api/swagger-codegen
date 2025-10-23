//package io.swagger.codegen.languages;
//
//import org.testng.Assert;
//import org.testng.annotations.Test;
//
//public class RubyClientCodegenTest {
//
//    @Test
//    public void testShouldOverwriteWithPathTraversal() {
//        RubyClientCodegen codegen = new RubyClientCodegen();
//        Assert.assertThrows(
//                "shouldOverwrite should throw SecurityException for suspicious path",
//                SecurityException.class,
//                () -> codegen.shouldOverwrite("../../../etc/passwd")
//        );
//    }
//}
