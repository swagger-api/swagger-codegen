package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Body2;
import io.swagger.model.Body3;
import io.swagger.model.Body4;
import io.swagger.model.Body5;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link FakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
public interface FakeApiDelegate {

    /**
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    ResponseEntity<Boolean> fakeOuterBooleanSerialize( Boolean  body);

    /**
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    ResponseEntity<OuterComposite> fakeOuterCompositeSerialize( OuterComposite  body);

    /**
     * @see FakeApi#fakeOuterNumberSerialize
     */
    ResponseEntity<BigDecimal> fakeOuterNumberSerialize( BigDecimal  body);

    /**
     * @see FakeApi#fakeOuterStringSerialize
     */
    ResponseEntity<String> fakeOuterStringSerialize( String  body);

    /**
     * @see FakeApi#testClientModel
     */
    ResponseEntity<Client> testClientModel( Client  body);

    /**
     * @see FakeApi#testEndpointParameters
     */
    ResponseEntity<Void> testEndpointParameters( Body2  body);

    /**
     * @see FakeApi#testEnumParameters
     */
    ResponseEntity<Void> testEnumParameters( List<String>  enumHeaderStringArray,
         String  enumHeaderString,
         List<String>  enumQueryStringArray,
         String  enumQueryString,
         Integer  enumQueryInteger);

    /**
     * @see FakeApi#testEnumRequestBody
     */
    ResponseEntity<Void> testEnumRequestBody( Body4  body);

    /**
     * @see FakeApi#testInlineAdditionalProperties
     */
    ResponseEntity<Void> testInlineAdditionalProperties( Map<String, String>  body);

    /**
     * @see FakeApi#testJsonFormData
     */
    ResponseEntity<Void> testJsonFormData( Body5  body);

}
