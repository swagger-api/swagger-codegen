package io.swagger.api.impl;

import java.math.BigDecimal;
import io.swagger.model.Client;
import org.joda.time.LocalDate;

import io.swagger.api.FakeApi;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;

import java.util.List;

@Path("/fake")

@Api(description = "the fake API")




public class FakeApiServiceImpl implements FakeApi {

    public Response testClientModel(Client body) {
        return Response.ok().entity("magic!").build();
    }

    public Response testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, javax.xml.datatype.XMLGregorianCalendar dateTime, String password, String paramCallback) {
        return Response.ok().entity("magic!").build();
    }

    public Response testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) {
        return Response.ok().entity("magic!").build();
    }
}

