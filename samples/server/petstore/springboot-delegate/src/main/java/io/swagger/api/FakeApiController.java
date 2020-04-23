package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Body2;
import io.swagger.model.Body3;
import io.swagger.model.Body4;
import io.swagger.model.Body5;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;
import io.swagger.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;
@Controller
public class FakeApiController implements FakeApi {

    private final FakeApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeApiController(FakeApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Boolean> fakeOuterBooleanSerialize(@ApiParam(value = "Input boolean as post body"  )  @Valid @RequestBody Boolean body
) {
        return delegate.fakeOuterBooleanSerialize(body);
    }

    public ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(@ApiParam(value = "Input composite as post body"  )  @Valid @RequestBody OuterComposite body
) {
        return delegate.fakeOuterCompositeSerialize(body);
    }

    public ResponseEntity<BigDecimal> fakeOuterNumberSerialize(@ApiParam(value = "Input number as post body"  )  @Valid @RequestBody BigDecimal body
) {
        return delegate.fakeOuterNumberSerialize(body);
    }

    public ResponseEntity<String> fakeOuterStringSerialize(@ApiParam(value = "Input string as post body"  )  @Valid @RequestBody String body
) {
        return delegate.fakeOuterStringSerialize(body);
    }

    public ResponseEntity<Client> testClientModel(@ApiParam(value = "client model" ,required=true )  @Valid @RequestBody Client body
) {
        return delegate.testClientModel(body);
    }

    public ResponseEntity<Void> testEndpointParameters(@ApiParam(value = "" ,required=true )  @Valid @RequestBody Body2 body
) {
        return delegate.testEndpointParameters(body);
    }

    public ResponseEntity<Void> testEnumParameters(@ApiParam(value = "Header parameter enum test (string array)" , allowableValues=">, $") @RequestHeader(value="enum_header_string_array", required=false) List<String> enumHeaderStringArray
,@ApiParam(value = "Header parameter enum test (string)" , allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @RequestHeader(value="enum_header_string", required=false) String enumHeaderString
,@ApiParam(value = "Query parameter enum test (string array)", allowableValues = ">, $") @Valid @RequestParam(value = "enum_query_string_array", required = false) List<String> enumQueryStringArray
,@ApiParam(value = "Query parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @Valid @RequestParam(value = "enum_query_string", required = false, defaultValue="-efg") String enumQueryString
,@ApiParam(value = "Query parameter enum test (double)", allowableValues = "1, -2") @Valid @RequestParam(value = "enum_query_integer", required = false) Integer enumQueryInteger
) {
        return delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger);
    }

    public ResponseEntity<Void> testEnumRequestBody(@ApiParam(value = ""  )  @Valid @RequestBody Body4 body
) {
        return delegate.testEnumRequestBody(body);
    }

    public ResponseEntity<Void> testInlineAdditionalProperties(@ApiParam(value = "request body" ,required=true )  @Valid @RequestBody Map<String, String> body
) {
        return delegate.testInlineAdditionalProperties(body);
    }

    public ResponseEntity<Void> testJsonFormData(@ApiParam(value = "" ,required=true )  @Valid @RequestBody Body5 body
) {
        return delegate.testJsonFormData(body);
    }

}
