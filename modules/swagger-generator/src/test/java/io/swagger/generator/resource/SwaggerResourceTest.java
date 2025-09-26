package io.swagger.generator.resource;

import org.testng.annotations.Test;

public class SwaggerResourceTest {

    @Test(expectedExceptions = SecurityException.class)
    public void testDownloadFileWithPathTraversal() throws Exception {
        SwaggerResource resource = new SwaggerResource();

        io.swagger.generator.model.Generated generated = new io.swagger.generator.model.Generated();
        generated.setFilename("../../../etc/passwd");

        java.lang.reflect.Field fileMapField = SwaggerResource.class.getDeclaredField("fileMap");
        fileMapField.setAccessible(true);
        @SuppressWarnings("unchecked")
        java.util.Map<String, io.swagger.generator.model.Generated> fileMap =
            (java.util.Map<String, io.swagger.generator.model.Generated>) fileMapField.get(null);
        fileMap.put("test-file-id", generated);

        try {
            resource.downloadFile("test-file-id");
        } finally {
            fileMap.remove("test-file-id");
        }
    }
}
