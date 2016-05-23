package io.swagger.petstore.test;

import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

import io.swagger.TestUtils;
import io.swagger.client.api.UserApi;
import io.swagger.client.auth.ApiKeyAuth;
import io.swagger.client.model.User;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class UserApiTest {
    UserApi api = null;

    @Before
    public void setup() {
        api = new UserApi();
        // setup authentication
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) api.getApiClient().getAuthentication("api_key");
        apiKeyAuth.setApiKey("special-key");
    }

    @Test
    public void testCreateUser() throws Exception {
        User user = createUser();

        api.createUser(user);

        User fetched = api.getUserByName(user.getUsername());
        assertEquals(user.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithArray() throws Exception {
        User user1 = createUser();
        user1.setUsername("user" + user1.getId());
        User user2 = createUser();
        user2.setUsername("user" + user2.getId());

        api.createUsersWithArrayInput(Arrays.asList(new User[]{user1, user2}));

        User fetched = api.getUserByName(user1.getUsername());
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithList() throws Exception {
        User user1 = createUser();
        user1.setUsername("user" + user1.getId());
        User user2 = createUser();
        user2.setUsername("user" + user2.getId());

        api.createUsersWithListInput(Arrays.asList(new User[]{user1, user2}));

        User fetched = api.getUserByName(user1.getUsername());
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testLoginUser() throws Exception {
        User user = createUser();
        api.createUser(user);

        String token = api.loginUser(user.getUsername(), user.getPassword());
        assertTrue(token.startsWith("logged in user session:"));
    }

    @Test
    public void logoutUser() throws Exception {
        api.logoutUser();
    }

    private User createUser() {
        User user = new User();
        user.setId(TestUtils.nextId());
        user.setUsername("fred" + user.getId());
        user.setFirstName("Fred");
        user.setLastName("Meyer");
        user.setEmail("fred@fredmeyer.com");
        user.setPassword("xxXXxx");
        user.setPhone("408-867-5309");
        user.setUserStatus(123);

        return user;
    }
}
