package io.swagger.codegen.v3.service;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.util.Json;
import io.swagger.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.InputStream;
import java.util.List;

public class GeneratorServiceTest {

    @Test(description = "test generator service with java 3.0")
    public void testGeneratorServiceJava3() {

        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.SERVER)
                .lang("jaxrs-jersey")
                .spec(loadSpecAsNode("3_0_0/petstore.json", false, false))
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
    }

    @Test
    public void testNoModel() throws Exception{

        String path = getTmpFolder().getAbsolutePath();
        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.SERVER)
                .lang("jaxrs-jersey")
                .spec(loadSpecAsNode("3_0_0/noModel.yaml", true, false))
                .options(
                        new Options()
                                .outputDir(path)
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        System.out.println(path);
        Assert.assertFalse(files.isEmpty());
        for (File f: files) {
            String relPath = f.getAbsolutePath().substring(path.length());
            if ("/src/main/java/io/swagger/api/impl/FooApiServiceImpl.java".equals(relPath) ||
                    "/src/gen/java/io/swagger/api/FooApiService.java".equals(relPath) ||
                    "/src/gen/java/io/swagger/api/FooApi.java".equals(relPath)) {
                Assert.assertFalse(FileUtils.readFileToString(f).contains("import io.swagger.model"));
            }
        }

    }

    @Test(description = "test generator service with java 2.0")
    public void testGeneratorServiceJava2() {

        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V2)
                .type(GenerationRequest.Type.SERVER)
                .lang("jaxrs")
                .spec(loadSpecAsNode("2_0/petstore.json", false, true))
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
    }

    @Test(description = "test generator service with java 3.0 and spec 2.0")
    public void testGeneratorServiceJava3Spec2() {

        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.SERVER)
                .lang("jaxrs-jersey")
                .spec(loadSpecAsNode("2_0/petstore.json", false, true))
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
    }

    @Test(description = "test generator service with java 3.0, spec as ref to file")
    public void testGeneratorServiceJava3RefSpec() {
        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.SERVER)
                .lang("jaxrs-jersey")
                .specURL("3_0_0/petstore.json")
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
    }


    @Test(description = "test generator service with java client 3.0")
    public void testGeneratorServiceJavaClient3() {

        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.CLIENT)
                .lang("java")
                .spec(loadSpecAsNode("3_0_0/petstore.json", false, false))
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                                .artifactId("swagger-petstore-jersey2")
                                .library("jersey2")
                                .addAdditionalProperty("useRuntimeException", true)
                                .addAdditionalProperty("useRxJava", true)
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
    }

    @Test(description = "test boolean additional properties")
    public void testGeneratorServiceBooleanAdditionalProperties() throws Exception {

        String path = getTmpFolder().getAbsolutePath();
        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.CLIENT)
                .lang("java")
                .spec(loadSpecAsNode("3_0_0/swos92.yaml", true, false))
                .options(
                        new Options()
                            .outputDir(path)
                            .artifactId("swagger-petstore-jersey2")
                            .library("jersey2")
                            .addAdditionalProperty("useRuntimeException", true)
                            .addAdditionalProperty("useRxJava", true)
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
        for (File f: files) {
            String relPath = f.getAbsolutePath().substring(path.length());
            if ("/src/main/java/io/swagger/client/model/Product.java".equals(relPath)) {
                Assert.assertTrue(FileUtils.readFileToString(f).contains("Map<String, Object> foo = null"));
            }
        }

    }

    @Test(description = "test generator service with java client 2.0")
    public void testGeneratorServiceJavaClient2() {
        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V2)
                .type(GenerationRequest.Type.CLIENT)
                .lang("java")
                .spec(loadSpecAsNode("2_0/petstore.json", false, true))
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                                .artifactId("swagger-petstore-jersey2")
                                .library("jersey2")
                );
        List<File> files = new GeneratorService().generationRequest(request).generate();
        Assert.assertFalse(files.isEmpty());
    }

    protected static File getTmpFolder() {
        try {
            File outputFolder = File.createTempFile("codegentest-", "-tmp");
            outputFolder.delete();
            outputFolder.mkdir();
            outputFolder.deleteOnExit();
            return outputFolder;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    protected JsonNode loadSpecAsNode(final String file, boolean yaml, boolean v2) {
        InputStream in = null;
        String s = "";
        try {
            in = getClass().getClassLoader().getResourceAsStream(file);
            if (yaml) {
                if (v2) {
                    return Yaml.mapper().readTree(in);
                } else {
                    return io.swagger.v3.core.util.Yaml.mapper().readTree(in);
                }
            }
            if (v2) {
                return Json.mapper().readTree(in);
            }
            return io.swagger.v3.core.util.Json.mapper().readTree(in);
        } catch (Exception e) {
            throw new RuntimeException("could not load file " + file);
        } finally {
            IOUtils.closeQuietly(in);
        }
    }

    protected OpenAPI deserializeOpenAPI(final String file, boolean yaml, boolean v2) {
        InputStream in = null;
        String s = "";
        try {
            in = getClass().getClassLoader().getResourceAsStream(file);
            if (yaml) {
                return io.swagger.v3.core.util.Yaml.mapper().readValue(in, OpenAPI.class);
            }
            return io.swagger.v3.core.util.Json.mapper().readValue(in, OpenAPI.class);
        } catch (Exception e) {
            throw new RuntimeException("could not load file " + file);
        } finally {
            IOUtils.closeQuietly(in);
        }
    }

    private static OpenAPI parseOpenAPI(String path) {
        final ParseOptions options = new ParseOptions();
        options.setFlatten(Boolean.TRUE);
        return new OpenAPIV3Parser()
                .readLocation(path, null, options)
                .getOpenAPI();
    }
}
