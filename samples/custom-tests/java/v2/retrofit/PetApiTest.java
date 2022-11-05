package io.swagger.client.api;

import io.swagger.client.ApiClient;
import java.io.File;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import java.net.HttpURLConnection;
import org.junit.AfterClass;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.*;

/**
 * API tests for PetApi
 */
public class PetApiTest {

    private PetApi api;

    private static WireMockServer wireMockServer;

    @Before
    public void setup() {
        wireMockServer = new WireMockServer(WireMockConfiguration.wireMockConfig().dynamicPort());
        wireMockServer.start();

        configureFor(wireMockServer.port());

        ApiClient apiClient = new ApiClient();
        apiClient.getAdapterBuilder().setEndpoint("http://localhost:" + wireMockServer.port());
        api = apiClient.createService(PetApi.class);


        configureFor(wireMockServer.port());

        stubFor(post(urlPathMatching("/pet"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(put(urlPathMatching("/pet"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(get(urlPathMatching("/pet/10"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)
                        .withBody("{\"id\":10,\"category\":{\"id\":7007,\"name\":\"string\"},\"name\":\"doggie\",\"photoUrls\":[\"string\"],\"tags\":[{\"id\":7007,\"name\":\"string\"}],\"status\":\"available\"}")));

        stubFor(delete(urlPathMatching("/pet/10"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(get(urlPathMatching("/pet/findByStatus"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)
                        .withBody("[{\"id\":7007,\"category\":{\"id\":7007,\"name\":\"string\"},\"name\":\"doggie\",\"photoUrls\":[\"string\"],\"tags\":[{\"id\":7007,\"name\":\"string\"}],\"status\":\"available\"},{\"id\":44550,\"category\":{\"id\":0,\"name\":\"string\"},\"name\":\"SuperHund\",\"photoUrls\":[\"string\"],\"tags\":[{\"id\":0,\"name\":\"string\"}],\"status\":\"available\"},{\"id\":3377,\"category\":{\"id\":0,\"name\":\"Lambo\"},\"name\":\"Lambo\",\"photoUrls\":[\"string\"],\"tags\":[{\"id\":0,\"name\":\"Lambo\"}],\"status\":\"available\"}]")
                ));
        
    }

    @AfterClass
    public static void tearDown() {
        wireMockServer.stop();
    }

    
    /**
     * Add a new pet to the store
     *
     * 
     */
    @Test
    public void addPetTest() {
        Pet body = new Pet()
                .id(10L)
                .name("doggie")
                .status(Pet.StatusEnum.AVAILABLE)
                .photoUrls(Arrays.asList("http://some.pic"));
        api.addPet(body);

        verify(exactly(1), postRequestedFor(urlEqualTo("/pet")));
    }
    
    /**
     * Deletes a pet
     *
     * 
     */
    @Test
    public void deletePetTest() {
        Long petId = 10L;
        String apiKey = "abc123";
        api.deletePet(petId, apiKey);

        verify(exactly(1), deleteRequestedFor(urlEqualTo("/pet/10")));
    }
    
    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     */
    @Test
    public void findPetsByStatusTest() {
        List<String> status = Arrays.asList("available");
        List<Pet> response = api.findPetsByStatus(status);

        Assert.assertNotNull(response);
        Assert.assertFalse(response.isEmpty());

        verify(exactly(1), getRequestedFor(urlEqualTo("/pet/findByStatus?status=available")));
    }
    
    /**
     * Find pet by ID
     *
     * Returns a single pet
     */
    @Test
    public void getPetByIdTest() {
        Long petId = 10L;
        Pet response = api.getPetById(petId);

        Assert.assertNotNull(response);
        Assert.assertEquals(10L, response.getId().longValue());
        Assert.assertEquals("doggie", response.getName());

        verify(exactly(1), getRequestedFor(urlEqualTo("/pet/10")));
    }
    
    /**
     * Update an existing pet
     *
     * 
     */
    @Test
    public void updatePetTest() {
        Pet body = new Pet()
                .id(10L)
                .name("doggie")
                .status(Pet.StatusEnum.AVAILABLE)
                .photoUrls(Arrays.asList("http://some.pic"));

        api.updatePet(body);

        verify(exactly(1), putRequestedFor(urlEqualTo("/pet")));
    }
}
