package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.Dog;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
public abstract class DogApiService {
      public abstract Response addDog(Dog body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response deleteDog(Long dogId,String apiKey,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getDogById(Long dogId,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updateDog(Dog body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updateDogWithForm(Long dogId,String name,String status,SecurityContext securityContext)
      throws NotFoundException;
}
