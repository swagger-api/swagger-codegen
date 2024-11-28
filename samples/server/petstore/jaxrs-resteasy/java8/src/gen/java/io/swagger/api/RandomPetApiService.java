package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import io.swagger.model.SinglePetResponse;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

public interface RandomPetApiService {
      Response getRandomPet(SecurityContext securityContext)
      throws NotFoundException;
}
