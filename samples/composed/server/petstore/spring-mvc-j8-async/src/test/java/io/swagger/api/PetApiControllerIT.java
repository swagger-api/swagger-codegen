package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import java.util.*;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;

import org.testng.annotations.Test;
import static org.junit.Assert.assertEquals;

/**
 * Test class to verify that GET endpoints on generated project are reached.
 */
public class PetApiControllerIT {

    @Test
    public void findPetsByStatusTest() throws Exception {
        final String requestURL = "http://localhost:8002//pet/findByStatus?status=status_example";
        final HttpClient client = HttpClientBuilder.create().build();
        final HttpResponse response = client.execute(new HttpGet(requestURL));
        assertEquals(response.getStatusLine().getStatusCode(), 501);
    }
    @Test
    public void findPetsByTagsTest() throws Exception {
        final String requestURL = "http://localhost:8002//pet/findByTags?tags=tags_example";
        final HttpClient client = HttpClientBuilder.create().build();
        final HttpResponse response = client.execute(new HttpGet(requestURL));
        assertEquals(response.getStatusLine().getStatusCode(), 501);
    }
    @Test
    public void getPetByIdTest() throws Exception {
        final String requestURL = "http://localhost:8002//pet/789";
        final HttpClient client = HttpClientBuilder.create().build();
        final HttpResponse response = client.execute(new HttpGet(requestURL));
        assertEquals(response.getStatusLine().getStatusCode(), 501);
    }
}
