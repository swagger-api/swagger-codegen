package io.swagger.v3.generator.online;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.v3.service.GenerationRequest;
import io.swagger.codegen.v3.service.Options;
import io.swagger.oas.inflector.models.RequestContext;
import io.swagger.oas.inflector.models.ResponseContext;
import io.swagger.v3.core.util.Json;
import org.apache.commons.io.FileUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.ws.rs.core.MediaType;
import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;

public class GeneratorControllerTest {
    @Test
    public void generateJava() throws Exception {
        String json = FileUtils.readFileToString(new File("src/test/resources/petstore-oas3.json"));
        JsonNode node = Json.mapper().readTree(json);
        Map<String, Object> spec = Json.mapper().convertValue(node, LinkedHashMap.class);


        GenerationRequest generationRequest = new GenerationRequest()
                .spec(spec)
                .lang("java")
                .options(new Options()
                        .outputDir("testout")
                        .addAdditionalProperty("outputFolder", "testoutputFolder")
                        .addAdditionalProperty("useRuntimeException", true)
                        .addAdditionalProperty("useRxJava", true));

        GeneratorController g = new GeneratorController();
        RequestContext r = new RequestContext();
        ResponseContext rr = g.generate(r, generationRequest);
        Assert.assertEquals(rr.getStatus(), 200);
        Assert.assertEquals(rr.getContentType(), MediaType.APPLICATION_OCTET_STREAM_TYPE);
        Assert.assertTrue(rr.getHeaders().getFirst("Content-Disposition").contains(" filename=\"java-client-generated.zip\""));
    }
}
