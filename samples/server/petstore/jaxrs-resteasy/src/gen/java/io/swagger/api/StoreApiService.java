package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;


import io.swagger.model.Order;
<<<<<<< HEAD
import java.util.Map;
=======
>>>>>>> upstream/master

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

<<<<<<< HEAD
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-03-16T14:27:58.108+08:00")
public abstract class StoreApiService {
  
      public abstract Response deleteOrder(String orderId,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response findOrdersByStatus(String status,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response getInventory(SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response getInventoryInObject(SecurityContext securityContext)
=======
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-04-15T18:10:39.667+08:00")
public abstract class StoreApiService {
      public abstract Response deleteOrder(String orderId,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getInventory(SecurityContext securityContext)
>>>>>>> upstream/master
      throws NotFoundException;
      public abstract Response getOrderById(Long orderId,SecurityContext securityContext)
      throws NotFoundException;
<<<<<<< HEAD
  
=======
>>>>>>> upstream/master
      public abstract Response placeOrder(Order body,SecurityContext securityContext)
      throws NotFoundException;
}
