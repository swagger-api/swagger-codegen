package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import io.swagger.model.AllPetsResponse;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

public interface AllPetsApiService {
      Response getAllPets(SecurityContext securityContext)
      throws NotFoundException;
}
