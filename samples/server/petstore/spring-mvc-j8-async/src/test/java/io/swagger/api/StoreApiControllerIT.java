package io.swagger.api;
import java.util.Map;
import io.swagger.model.Order;
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
public class StoreApiControllerIT {

    @Test
        public void getInventoryTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/store/inventory";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
    @Test
        public void getOrderByIdTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/store/order/789";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
}