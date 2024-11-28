package io.swagger.api;

import io.swagger.model.User;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

@RunWith(SpringRunner.class)
@SpringBootTest
public class UserApiTest {

    @Autowired
    private UserApi api;

    @Test
    public void createUserTest() throws Exception {
        User body = new User();
        ResponseEntity<User> responseEntity = api.createUser(body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void createUsersWithListInputTest() throws Exception {
        List<User> body = Arrays.asList(new User());
        ResponseEntity<User> responseEntity = api.createUsersWithListInput(body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void deleteUserTest() throws Exception {
        String username = "username_example";
        ResponseEntity<Void> responseEntity = api.deleteUser(username);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void getUserByNameTest() throws Exception {
        String username = "username_example";
        ResponseEntity<User> responseEntity = api.getUserByName(username);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void loginUserTest() throws Exception {
        String username = "username_example";
        String password = "password_example";
        ResponseEntity<String> responseEntity = api.loginUser(username, password);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void logoutUserTest() throws Exception {
        ResponseEntity<Void> responseEntity = api.logoutUser();
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void updateUserTest() throws Exception {
        String username = "username_example";
        User body = new User();
        ResponseEntity<Void> responseEntity = api.updateUser(username, body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }
}
