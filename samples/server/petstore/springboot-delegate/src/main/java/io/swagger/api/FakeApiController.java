package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import io.swagger.model.EnumFormBody;
import io.swagger.model.FakeBody;
import io.swagger.model.FakeBody1;
import io.swagger.model.FakeJsonFormDataBody;
import io.swagger.model.OuterComposite;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

@RestController
public class FakeApiController implements FakeApi {

    private final FakeApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeApiController(FakeApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Boolean> fakeOuterBooleanSerialize(@Parameter(in = ParameterIn.DEFAULT, description = "Input boolean as post body", schema=@Schema()) @Valid @RequestBody Boolean body) {
        return delegate.fakeOuterBooleanSerialize(body);
    }

    public ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(@Parameter(in = ParameterIn.DEFAULT, description = "Input composite as post body", schema=@Schema()) @Valid @RequestBody OuterComposite body) {
        return delegate.fakeOuterCompositeSerialize(body);
    }

    public ResponseEntity<BigDecimal> fakeOuterNumberSerialize(@Parameter(in = ParameterIn.DEFAULT, description = "Input number as post body", schema=@Schema()) @Valid @RequestBody BigDecimal body) {
        return delegate.fakeOuterNumberSerialize(body);
    }

    public ResponseEntity<String> fakeOuterStringSerialize(@Parameter(in = ParameterIn.DEFAULT, description = "Input string as post body", schema=@Schema()) @Valid @RequestBody String body) {
        return delegate.fakeOuterStringSerialize(body);
    }

    public ResponseEntity<Client> testClientModel(@Parameter(in = ParameterIn.DEFAULT, description = "client model", required=true, schema=@Schema()) @Valid @RequestBody Client body) {
        return delegate.testClientModel(body);
    }

    public ResponseEntity<Void> testEndpointParameters(@Parameter(in = ParameterIn.DEFAULT, description = "", required=true, schema=@Schema()) @Valid @RequestBody FakeBody body) {
        return delegate.testEndpointParameters(body);
    }

    public ResponseEntity<Void> testEnumParameters(@Parameter(in = ParameterIn.HEADER, description = "Header parameter enum test (string array)" ,schema=@Schema(allowableValues={ ">", "$" }
)) @RequestHeader(value="enum_header_string_array", required=false) List<String> enumHeaderStringArray,@Parameter(in = ParameterIn.HEADER, description = "Header parameter enum test (string)" ,schema=@Schema(allowableValues={ "_abc", "-efg", "(xyz)" }
, defaultValue="-efg")) @RequestHeader(value="enum_header_string", required=false) String enumHeaderString,@Parameter(in = ParameterIn.QUERY, description = "Query parameter enum test (string array)" ,schema=@Schema(allowableValues={ ">", "$" }
)) @Valid @RequestParam(value = "enum_query_string_array", required = false) List<String> enumQueryStringArray,@Parameter(in = ParameterIn.QUERY, description = "Query parameter enum test (string)" ,schema=@Schema(allowableValues={ "_abc", "-efg", "(xyz)" }
, defaultValue="-efg")) @Valid @RequestParam(value = "enum_query_string", required = false, defaultValue="-efg") String enumQueryString,@Parameter(in = ParameterIn.QUERY, description = "Query parameter enum test (double)" ,schema=@Schema(allowableValues={ "1", "-2" }
)) @Valid @RequestParam(value = "enum_query_integer", required = false) Integer enumQueryInteger) {
        return delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger);
    }

    public ResponseEntity<Void> testEnumRequestBody(@Parameter(in = ParameterIn.DEFAULT, description = "", schema=@Schema()) @Valid @RequestBody EnumFormBody body) {
        return delegate.testEnumRequestBody(body);
    }

    public ResponseEntity<Void> testInlineAdditionalProperties(@Parameter(in = ParameterIn.DEFAULT, description = "request body", required=true, schema=@Schema()) @Valid @RequestBody Map<String, String> body) {
        return delegate.testInlineAdditionalProperties(body);
    }

    public ResponseEntity<Void> testJsonFormData(@Parameter(in = ParameterIn.DEFAULT, description = "", required=true, schema=@Schema()) @Valid @RequestBody FakeJsonFormDataBody body) {
        return delegate.testJsonFormData(body);
    }

}
