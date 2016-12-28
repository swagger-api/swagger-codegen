package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;

import java.util.Map;
import io.swagger.model.Order;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "io.swagger.codegen.languages.JavaJAXRSCXFCDIServerCodegen", date = "2016-12-28T16:00:21.045+07:00")
public interface StoreApiService {
      public Response deleteOrder(String orderId, SecurityContext securityContext);
      public Response getInventory(SecurityContext securityContext);
      public Response getOrderById(Long orderId, SecurityContext securityContext);
      public Response placeOrder(Order body, SecurityContext securityContext);
}
