package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.*;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import java.util.Map;
import io.swagger.model.Order;

import java.util.List;

import java.io.InputStream;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@RequestScoped
@javax.annotation.Generated(value = "io.swagger.codegen.languages.JavaJAXRSCXFCDIServerCodegen", date = "2017-01-05T14:32:58.862+11:00")
public class StoreApiServiceImpl implements StoreApiService {
      @Override
      public Response deleteOrder(String orderId, SecurityContext securityContext) {
      // do some magic!
      return Response.ok().entity("magic!").build();
  }
      @Override
      public Response getInventory(SecurityContext securityContext) {
      // do some magic!
      return Response.ok().entity("magic!").build();
  }
      @Override
      public Response getOrderById(Long orderId, SecurityContext securityContext) {
      // do some magic!
      return Response.ok().entity("magic!").build();
  }
      @Override
      public Response placeOrder(Order body, SecurityContext securityContext) {
      // do some magic!
      return Response.ok().entity("magic!").build();
  }
}
