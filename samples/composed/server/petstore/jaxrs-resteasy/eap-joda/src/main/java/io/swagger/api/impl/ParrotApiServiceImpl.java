package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.*;

import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

public class ParrotApiServiceImpl implements ParrotApi {
      public Response addParrot(Body2 body,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getParrots(SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response updateParrots(Body1 body,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
}
