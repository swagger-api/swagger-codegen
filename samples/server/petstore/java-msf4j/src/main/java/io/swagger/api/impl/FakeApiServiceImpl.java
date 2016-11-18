package io.swagger.api.impl;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.ws.rs.core.Response;

import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.Client;


public class FakeApiServiceImpl extends FakeApiService {
    @Override
    public Response testClientModel(Client body ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, Date date, Date dateTime, String password, String paramCallback ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
    @Override
    public Response testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, BigDecimal enumQueryInteger, Double enumQueryDouble ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
