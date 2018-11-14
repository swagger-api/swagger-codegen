package io.swagger.api;
import java.math.BigDecimal;
import io.swagger.model.Client;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import io.swagger.model.OuterComposite;
import io.swagger.model.User;
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
public class FakeApiControllerIntegrationTest {
@Autowired
private FakeApi api;
        @Test
        public void fakeOuterBooleanSerializeTest() throws Exception {
            Boolean body = true;
            ResponseEntity<Boolean> responseEntity = api.fakeOuterBooleanSerialize(body);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void fakeOuterCompositeSerializeTest() throws Exception {
            OuterComposite body = new OuterComposite();
            ResponseEntity<OuterComposite> responseEntity = api.fakeOuterCompositeSerialize(body);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void fakeOuterNumberSerializeTest() throws Exception {
            BigDecimal body = new BigDecimal();
            ResponseEntity<BigDecimal> responseEntity = api.fakeOuterNumberSerialize(body);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void fakeOuterStringSerializeTest() throws Exception {
            String body = "body_example";
            ResponseEntity<String> responseEntity = api.fakeOuterStringSerialize(body);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void testBodyWithQueryParamsTest() throws Exception {
            User body = new User();
            String query = "query_example";
            ResponseEntity<Void> responseEntity = api.testBodyWithQueryParams(body, query);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void testClientModelTest() throws Exception {
            Client body = new Client();
            ResponseEntity<Client> responseEntity = api.testClientModel(body);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void testEndpointParametersTest() throws Exception {
            BigDecimal number = new BigDecimal();
            Double _double = 3.4D;
            String patternWithoutDelimiter = "patternWithoutDelimiter_example";
            byte[] _byte = B;
            Integer integer = 56;
            Integer int32 = 56;
            Long int64 = 789L;
            Float _float = 3.4F;
            String string = "string_example";
            byte[] binary = B;
            LocalDate date = new LocalDate();
            OffsetDateTime dateTime = new OffsetDateTime();
            String password = "password_example";
            String paramCallback = "paramCallback_example";
            ResponseEntity<Void> responseEntity = api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void testEnumParametersTest() throws Exception {
            List<String> enumFormStringArray = Arrays.asList("enumFormStringArray_example");
            String enumFormString = "-efg";
            List<String> enumHeaderStringArray = Arrays.asList("enumHeaderStringArray_example");
            String enumHeaderString = "-efg";
            List<String> enumQueryStringArray = Arrays.asList("enumQueryStringArray_example");
            String enumQueryString = "-efg";
            Integer enumQueryInteger = 56;
            Double enumQueryDouble = 3.4D;
            ResponseEntity<Void> responseEntity = api.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void testInlineAdditionalPropertiesTest() throws Exception {
            Object param = null;
            ResponseEntity<Void> responseEntity = api.testInlineAdditionalProperties(param);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
        @Test
        public void testJsonFormDataTest() throws Exception {
            String param = "param_example";
            String param2 = "param2_example";
            ResponseEntity<Void> responseEntity = api.testJsonFormData(param, param2);
            assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
        }
}