package io.swagger.api;

import io.swagger.model.Pet;
import io.swagger.util.Json;
import org.apache.http.*;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.*;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;

/**
* Tests to verify endpoint calls.
*/
public class PetApiIT {

    static final String DEFAULT_HOST = "http://localhost:8080";

    @Test
    public void addPetTest() throws Exception {
        final HttpClient client = HttpClientBuilder.create().build();
        HttpPost httpPost = new HttpPost(DEFAULT_HOST + "/pet");

        Pet pet = new Pet();
        pet.setName("test-name");
        pet.setPhotoUrls(new ArrayList<String>());

        HttpEntity entity = new StringEntity(Json.pretty(pet), "UTF-8");

        httpPost.setHeader("Content-Type", "application/json");
        httpPost.setEntity(entity);

        final HttpResponse response = client.execute(httpPost);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
    }

    @Test
    public void deletePetTest() throws Exception {
        final HttpClient client = HttpClientBuilder.create().build();
        HttpRequestBase requestBase = null;
        requestBase = new HttpDelete(DEFAULT_HOST + "/pet/0");
        final HttpResponse response = client.execute(requestBase);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
    }

    @Test
    public void findPetsByStatusTest() throws Exception {
        final HttpClient client = HttpClientBuilder.create().build();
        HttpRequestBase requestBase = null;
        requestBase = new HttpGet(DEFAULT_HOST + "/pet/findByStatus");
        final HttpResponse response = client.execute(requestBase);
        int responseCode = response.getStatusLine().getStatusCode();
        Assert.assertEquals(responseCode, 200);
    }
}