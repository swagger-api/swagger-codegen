package io.swagger.api;
import java.util.List;
import io.swagger.model.User;
import java.util.*;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import org.testng.annotations.Test;
import static org.junit.Assert.assertTrue;
/**
* Test class to verify that GET endpoints on generated project are reached.
*/
public class UserApiControllerIT {

    @Test
        public void getUserByNameTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/user/username_example";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
    @Test
        public void loginUserTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/user/login?username=username_example&password=password_example";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
    @Test
        public void logoutUserTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/user/logout";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
}