package io.swagger.api;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import io.swagger.api.*;
import io.swagger.model.Order;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJAXRSCXFCDIServerCodegen", date = "2016-10-11T07:40:42.070+01:00")
public interface StoreApiService {
      public Response deleteOrder(String orderId, SecurityContext securityContext);
      public Response getInventory(SecurityContext securityContext);
      public Response getOrderById(Long orderId, SecurityContext securityContext);
      public Response placeOrder(Order body, SecurityContext securityContext);
}
