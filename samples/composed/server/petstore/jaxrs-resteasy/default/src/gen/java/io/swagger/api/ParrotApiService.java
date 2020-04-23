package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import io.swagger.model.Body1;
import io.swagger.model.Body2;
import io.swagger.model.InlineResponse200;
import io.swagger.model.InlineResponse2001;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

public interface ParrotApiService {
      Response addParrot(Body2 body,SecurityContext securityContext)
      throws NotFoundException;
      Response getParrots(SecurityContext securityContext)
      throws NotFoundException;
      Response updateParrots(Body1 body,SecurityContext securityContext)
      throws NotFoundException;
}
