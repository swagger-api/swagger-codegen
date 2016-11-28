package io.swagger.api;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.Client;


public abstract class FakeApiService {
      public abstract Response testClientModel(Client body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response testEndpointParameters(BigDecimal number,Double _double,String patternWithoutDelimiter,byte[] _byte,Integer integer,Integer int32,Long int64,Float _float,String string,byte[] binary,Date date,Date dateTime,String password,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response testEnumParameters(List<String> enumFormStringArray,String enumFormString,List<String> enumHeaderStringArray,String enumHeaderString,List<String> enumQueryStringArray,String enumQueryString,BigDecimal enumQueryInteger,Double enumQueryDouble,SecurityContext securityContext)
      throws NotFoundException;
}
