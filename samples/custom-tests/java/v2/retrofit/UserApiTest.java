package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.model.User;
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
 * API tests for UserApi
 */
public class UserApiTest {

    private UserApi api;

    private static WireMockServer wireMockServer;

    @Before
    public void setup() {
        wireMockServer = new WireMockServer(WireMockConfiguration.wireMockConfig().dynamicPort());
        wireMockServer.start();

        configureFor(wireMockServer.port());

        ApiClient apiClient = new ApiClient();
        apiClient.getAdapterBuilder().setEndpoint("http://localhost:" + wireMockServer.port());
        api = apiClient.createService(UserApi.class);


        stubFor(post(urlPathMatching("/user"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(post(urlPathMatching("/user/createWithArray"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(post(urlPathMatching("/user/createWithList"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(delete(urlPathMatching("/user/user1"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)));

        stubFor(get(urlPathMatching("/user/user1"))
                .willReturn(aResponse()
                        .withStatus(HttpURLConnection.HTTP_OK)
                        .withBody("{\"id\":10,\"username\":\"user1\"}")));
        
    }

    @AfterClass
    public static void tearDown() {
        wireMockServer.stop();
    }

    
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    @Test
    public void createUserTest() {
        User body = new User().id(10L).username("user1");
        api.createUser(body);

        verify(exactly(1), postRequestedFor(urlEqualTo("/user")));
    }
    
    /**
     * Creates list of users with given input array
     *
     * 
     */
    @Test
    public void createUsersWithArrayInputTest() {
        User user = new User().id(10L).username("user1");
        List<User> body = Arrays.asList(user);
        api.createUsersWithArrayInput(body);

        verify(exactly(1), postRequestedFor(urlEqualTo("/user/createWithArray")));
    }
    
    /**
     * Creates list of users with given input array
     *
     * 
     */
    @Test
    public void createUsersWithListInputTest() {
        User user = new User().id(10L).username("user1");
        List<User> body = Arrays.asList(user);
        api.createUsersWithListInput(body);

        verify(exactly(1), postRequestedFor(urlEqualTo("/user/createWithList")));
    }
    
    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @Test
    public void deleteUserTest() {
        String username = "user1";
        api.deleteUser(username);

        verify(exactly(1), deleteRequestedFor(urlEqualTo("/user/user1")));
    }
    
    /**
     * Get user by user name
     *
     * 
     */
    @Test
    public void getUserByNameTest() {
        String username = "user1";
        User response = api.getUserByName(username);

        Assert.assertEquals(10L, response.getId().longValue());
        Assert.assertEquals("user1", response.getUsername());

        verify(exactly(1), getRequestedFor(urlEqualTo("/user/user1")));
    }
}
