package io.swagger.api;

import org.apache.http.*;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.*;
import org.apache.http.impl.client.HttpClientBuilder;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
* API tests for StoreApi
*/
public class StoreApiIT {

    static final String DEFAULT_HOST = "http://localhost:8080";

    @Test
    public void deleteOrderTest() throws Exception {
        final HttpClient client = HttpClientBuilder.create().build();
        HttpRequestBase requestBase = null;
        requestBase = new HttpDelete(DEFAULT_HOST + "/store/order/0");
        final HttpResponse response = client.execute(requestBase);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
    }

    @Test
    public void getInventoryTest() throws Exception {
        final HttpClient client = HttpClientBuilder.create().build();
        HttpRequestBase requestBase = null;
        requestBase = new HttpGet(DEFAULT_HOST + "/store/inventory");
        final HttpResponse response = client.execute(requestBase);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
    }
}