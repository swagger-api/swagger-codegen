package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.*;

import io.swagger.model.AllPetsResponse;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

public class AllPetsApiServiceImpl implements AllPetsApi {
      public Response getAllPets(SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
}
