package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.Animal;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
public abstract class AnimalApiService {
      public abstract Response addAnimal(Animal body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response deleteAnimal(Long animalId,String apiKey,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getAnimalById(Long animalId,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updateAnimal(Animal body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updateAnimalWithForm(Long animalId,String name,String status,SecurityContext securityContext)
      throws NotFoundException;
}
