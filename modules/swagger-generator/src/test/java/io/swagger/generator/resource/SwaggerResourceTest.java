package io.swagger.generator.resource;

import io.swagger.codegen.utils.SecureFileUtils;
import io.swagger.generator.model.Generated;
import org.apache.commons.io.FileUtils;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.ws.rs.core.Response;
import java.io.File;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.assertFalse;
import static org.testng.AssertJUnit.assertEquals;

public class SwaggerResourceTest {

    private SwaggerResource resource;
    private Map<String, Generated> fileMap;

    @BeforeMethod
    public void setUp() throws Exception {
        resource = new SwaggerResource();

        fileMap = new HashMap<>();
        Field fm = SwaggerResource.class.getDeclaredField("fileMap");
        fm.setAccessible(true);
        fm.set(null, fileMap);
    }

    @AfterMethod
    public void after() {
        fileMap.clear();
    }

    @Test
    public void shouldReturnSuccessWhenDownloadFileAndBadRequestAfterSecondTry() throws Exception {
        File dir = new File("target/testng-gen1");
        File zip = new File(dir, "client.zip");
        FileUtils.write(zip, "TESTDATA", StandardCharsets.UTF_8);

        Generated g = new Generated();
        g.setFilename(zip.getAbsolutePath());
        g.setFriendlyName("clientX");

        fileMap.put("123", g);

        Response response = resource.downloadFile("123");

        assertEquals(200, response.getStatus());
        assertEquals("TESTDATA", new String((byte[]) response.getEntity()));
        assertFalse(zip.exists(), "File should be removed after download.");
        assertFalse(dir.exists(), "Directory should be removed after download.");

        Response response2 = resource.downloadFile("123");
        assertEquals(404, response2.getStatus());
    }

    @Test
    public void shouldReturnNotFoundWhenFileDoesntExist() {
        Response response = resource.downloadFile("nope");
        assertEquals(404, response.getStatus());
    }

    @Test(expectedExceptions = Exception.class)
    public void testDownloadFile_missingPhysicalFile_causes500() {
        Generated g = new Generated();
        g.setFilename("target/no_such_dir/file.zip");
        g.setFriendlyName("missing");

        fileMap.put("777", g);

        resource.downloadFile("777");
    }

    @Test(expectedExceptions = Exception.class)
    public void shouldPathValidationFailsWhenDownloadFile() throws Exception {
        try (MockedStatic<SecureFileUtils> mocked = Mockito.mockStatic(SecureFileUtils.class)) {

            mocked.when(() -> SecureFileUtils.validatePath(Mockito.anyString()))
                    .thenThrow(new RuntimeException("Invalid path"));

            File dir = new File("target/testng-gen2");
            File zip = new File(dir, "client.zip");
            FileUtils.write(zip, "XYZ", StandardCharsets.UTF_8);

            Generated g = new Generated();
            g.setFilename(zip.getAbsolutePath());
            g.setFriendlyName("clientY");

            fileMap.put("xyz", g);

            resource.downloadFile("xyz");
        }
    }

    @Test(expectedExceptions = SecurityException.class)
    public void testDownloadFileWithPathTraversal() throws Exception {

        io.swagger.generator.model.Generated generated = new io.swagger.generator.model.Generated();
        generated.setFilename("../../../etc/passwd");

        java.lang.reflect.Field fileMapField = SwaggerResource.class.getDeclaredField("fileMap");
        fileMapField.setAccessible(true);

        fileMap.put("test-file-id", generated);

        try {
            resource.downloadFile("test-file-id");
        } finally {
            fileMap.remove("test-file-id");
        }
    }
}
