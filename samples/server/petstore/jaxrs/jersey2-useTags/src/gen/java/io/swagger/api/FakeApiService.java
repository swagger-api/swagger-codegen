package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;

import java.math.BigDecimal;
import io.swagger.model.Body2;
import io.swagger.model.Body3;
import io.swagger.model.Body4;
import io.swagger.model.Body5;
import io.swagger.model.Client;
import io.swagger.model.OuterComposite;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
public abstract class FakeApiService {
    public abstract Response fakeOuterBooleanSerialize(Boolean body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterCompositeSerialize(OuterComposite body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterNumberSerialize(BigDecimal body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response fakeOuterStringSerialize(String body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testClientModel(Client body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testEndpointParameters(Body2 body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testEnumParameters(List<String> enumHeaderStringArray,String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testEnumRequestBody(Body4 body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testInlineAdditionalProperties(Map<String, String> body,SecurityContext securityContext) throws NotFoundException;
    public abstract Response testJsonFormData(Body5 body,SecurityContext securityContext) throws NotFoundException;
}
