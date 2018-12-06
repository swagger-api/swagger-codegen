package io.swagger.v3.generator.online;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.v3.service.GenerationRequest;
import io.swagger.codegen.v3.service.Options;
import io.swagger.v3.core.util.Json;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;

public class GeneratorControllerIT {

    protected final Logger LOGGER = LoggerFactory.getLogger(GeneratorControllerIT.class);
    static final String port = System.getProperty("JETTY_TEST_HTTP_PORT") == null ? "8080" : System.getProperty("JETTY_TEST_HTTP_PORT");
    static final String DEFAULT_HOST = "http://localhost:" + port + "/api";
    private HttpClient client = HttpClientBuilder.create().build();

    @Test
    public void testClientsV3() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/clients?version=V3"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("java"));
        Assert.assertTrue(jsonNode.toString().contains("html"));
    }

    @Test
    public void testDocumentationV3() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/documentation?version=V3"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("html"));
        Assert.assertFalse(jsonNode.toString().contains("java"));
    }

    @Test
    public void testClientsV2() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/clients?version=V2"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("java"));
        Assert.assertTrue(jsonNode.toString().contains("html"));
    }

    @Test
    public void testClientsV2NoDocs() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/clients?version=V2&clientOnly=true"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("java"));
        Assert.assertFalse(jsonNode.toString().contains("html"));
    }

    @Test
    public void testDocumentationV2() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/documentation?version=V2"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("html"));
    }

    @Test
    public void testServersV3() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/servers?version=V3"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("jaxrs-jersey"));
        Assert.assertFalse(jsonNode.toString().contains("html"));
    }

    @Test
    public void testServersV2() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/servers?version=V2"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("jaxrs"));
        Assert.assertFalse(jsonNode.toString().contains("html"));
    }

    @Test
    public void testTypeV2() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/server/V2"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("jaxrs"));
        Assert.assertFalse(jsonNode.toString().contains("html"));
    }

    @Test
    public void testTypeV3() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/server/V3"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("jaxrs-jersey"));
        Assert.assertFalse(jsonNode.toString().contains("html"));
    }

    @Test
    public void testMultiTypesV2() throws Exception {
        final HttpResponse response = client.execute(new HttpGet(DEFAULT_HOST + "/types?version=V2&types=client,documentation"));
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);

        String json = IOUtils.toString(response.getEntity().getContent());
        JsonNode jsonNode = Json.mapper().readTree(json);
        Assert.assertTrue(jsonNode.isArray());
        Assert.assertTrue(jsonNode.toString().contains("java"));
        Assert.assertTrue(jsonNode.toString().contains("html"));
        Assert.assertFalse(jsonNode.toString().contains("nodejs-server"));
    }

    @Test
    public void generateJava() throws Exception {
        String json = FileUtils.readFileToString(new File("src/test/resources/petstore-oas3.json"));
        JsonNode node = Json.mapper().readTree(json);
        Map<String, Object> spec = Json.mapper().convertValue(node, LinkedHashMap.class);


        GenerationRequest generationRequest = new GenerationRequest()
                .spec(spec)
                .lang("java")
                .options(new Options()
                        .addAdditionalProperty("useRuntimeException", true)
                        .addAdditionalProperty("useRxJava", true));
        HttpEntity entity = new StringEntity(Json.pretty(generationRequest), "UTF-8");

        HttpPost post = new HttpPost(DEFAULT_HOST + "/generate");
        post.setHeader("Content-Type", "application/json");
        post.setEntity(entity);

        final HttpResponse response = client.execute(post);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
        Assert.assertEquals(response.getFirstHeader("Content-Type").getValue(), "application/octet-stream");
        Assert.assertTrue(response.getFirstHeader("Content-Disposition").getValue().contains(" filename=\"java-client-generated.zip\""));
    }

    @Test
    public void generateHtml() throws Exception {
        HttpClient client = HttpClientBuilder.create().build();
        String json = FileUtils.readFileToString(new File("src/test/resources/petstore-oas3.json"));
        JsonNode node = Json.mapper().readTree(json);
        Map<String, Object> spec = Json.mapper().convertValue(node, LinkedHashMap.class);


        GenerationRequest generationRequest = new GenerationRequest()
                .spec(spec)
                .lang("html2");
        HttpEntity entity = new StringEntity(Json.pretty(generationRequest), "UTF-8");

        HttpPost post = new HttpPost(DEFAULT_HOST + "/generate");
        post.setHeader("Content-Type", "application/json");
        post.setEntity(entity);
        final HttpResponse response = client.execute(post);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
        Assert.assertEquals(response.getFirstHeader("Content-Type").getValue(), "application/octet-stream");
        Assert.assertTrue(response.getFirstHeader("Content-Disposition").getValue().contains(" filename=\"html2-documentation-generated.zip\""));
    }


}
