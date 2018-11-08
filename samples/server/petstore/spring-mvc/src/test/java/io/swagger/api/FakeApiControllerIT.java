package io.swagger.api;
import java.math.BigDecimal;
import io.swagger.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.model.OuterComposite;
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
public class FakeApiControllerIT {

    @Test
        public void testEnumParametersTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/fake?enumQueryStringArray=enumQueryStringArray_example&enumQueryString=-efg&enumQueryInteger=56";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
    @Test
        public void testJsonFormDataTest() throws Exception {
            final String requestURL = "http://localhost:8002/v2/fake/jsonFormData";
            final HttpClient client = HttpClientBuilder.create().build();
            final HttpResponse response = client.execute(new HttpGet(requestURL));
            assertTrue((response.getStatusLine().getStatusCode() == 415) || (response.getStatusLine().getStatusCode() == 501));
        }
}