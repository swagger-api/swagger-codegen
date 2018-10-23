package io.swagger.api;
import io.swagger.model.Client;
import java.util.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.Assert.assertEquals;

@RunWith(SpringRunner.class)
@SpringBootTest
public class AnotherFakeApiControllerIntegrationTest {
@Autowired
private AnotherFakeApi api;
        @Test
        public void testSpecialTagsTest() throws Exception {
            Client body = new Client();
            ResponseEntity<Client> responseEntity = api.testSpecialTags(body);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
}