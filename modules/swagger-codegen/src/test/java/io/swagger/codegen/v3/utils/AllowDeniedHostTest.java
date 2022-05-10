package io.swagger.codegen.v3.utils;


import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.verification.LoggedRequest;
import io.swagger.codegen.v3.service.GenerationRequest;
import io.swagger.codegen.v3.service.GeneratorService;
import io.swagger.codegen.v3.service.HostAccessControl;
import io.swagger.codegen.v3.service.Options;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.verify;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;

public class AllowDeniedHostTest {

    private static final int WIRE_MOCK_PORT = 9999;
    private static final String EXPECTED_ACCEPTS_HEADER = "application/json, application/yaml, */*";
    private static final String LOCALHOST = "localhost";
    private WireMockServer wireMockServer;


    @AfterMethod
    public void tearDown() throws Exception {
        wireMockServer.stop();
    }

    @BeforeMethod
    public void setUp() throws Exception {
        wireMockServer = new WireMockServer(WIRE_MOCK_PORT);
        wireMockServer.start();
        WireMock.configureFor(WIRE_MOCK_PORT);
    }

    @Test
    public void testAuthorizationHeaderAllowedHost() throws Exception {

        HostAccessControl allowedHostAccessControl = new HostAccessControl();
        allowedHostAccessControl.setHost("localhost");

        setupStub();

        final String headerValue = "foobar";
        final String headerName = "Authorization";
        final AuthorizationValue authorizationValue = new AuthorizationValue(headerName, headerValue, "header",
                url -> url.toString().startsWith("http://localhost"));

        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.SERVER)
                .lang("java")
                .specURL(getUrl())
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                                .authorizationValue(authorizationValue)
                                .allowedAuthHosts(Arrays.asList(allowedHostAccessControl))
                );

        new GeneratorService().generationRequest(request).generate();

        verify(getRequestedFor(urlEqualTo("/v2/pet/1"))
                .withHeader("Accept", equalTo(EXPECTED_ACCEPTS_HEADER))
                .withHeader(headerName, equalTo(headerValue))
        );
    }

    @Test
    public void testAuthorizationHeaderWithNonAllowedHost() throws Exception {

        HostAccessControl deniedHostAccessControl = new HostAccessControl();
        deniedHostAccessControl.setHost("localhost");

        setupStub();

        final String headerValue = "foobar";
        String authorization = "Authorization";
        final AuthorizationValue authorizationValue = new AuthorizationValue(authorization,
                headerValue, "header", u -> false);

        GenerationRequest request = new GenerationRequest();
        request
                .codegenVersion(GenerationRequest.CodegenVersion.V3)
                .type(GenerationRequest.Type.SERVER)
                .lang("java")
                .specURL(getUrl())
                .options(
                        new Options()
                                .outputDir(getTmpFolder().getAbsolutePath())
                                .authorizationValue(authorizationValue)
                                .deniedAuthHosts(Arrays.asList(deniedHostAccessControl))
                );

        new GeneratorService().generationRequest(request).generate();

        List<LoggedRequest> requests = WireMock.findAll(getRequestedFor(urlEqualTo("/v2/pet/1")));
        assertFalse(requests.get(0).containsHeader(authorization));
        assertEquals(requests.size(),2);

    }

    private String getUrl() {
        return String.format("http://%s:%d/v2/pet/1", LOCALHOST, WIRE_MOCK_PORT);
    }

    private String setupStub() {
        final String expectedBody = "openapi: 3.0.0\n" +
                "info:\n" +
                "  title: test\n" +
                "  version: \"0.0.1\"\n" +
                "\n" +
                "paths:\n" +
                "  '/contents/{id}':\n" +
                "    parameters:\n" +
                "      - name: id\n" +
                "        in: path\n" +
                "        description: test\n" +
                "        required: true\n" +
                "        schema:\n" +
                "          type: integer\n" +
                "  get:\n" +
                "    description: test\n" +
                "    responses:\n" +
                "      '200':\n" +
                "        description: OK\n" +
                "        schema: null\n" +
                "        $ref: '#/components/schemas/Content'\n" +
                "components:\n" +
                "  schemas:\n" +
                "    Content:\n" +
                "      type: object\n" +
                "      title: \t\ttest";

        stubFor(get(urlEqualTo("/v2/pet/1"))
                .willReturn(aResponse()
                        .withBody(expectedBody)
                        .withHeader("Content-Type", "application/json")
                ));
        return expectedBody;
    }

    protected static File getTmpFolder() {
        try {
            File outputFolder = Files.createTempFile("codegentest-", "-tmp").toFile();
            outputFolder.delete();
            outputFolder.mkdir();
            outputFolder.deleteOnExit();
            return outputFolder;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}

