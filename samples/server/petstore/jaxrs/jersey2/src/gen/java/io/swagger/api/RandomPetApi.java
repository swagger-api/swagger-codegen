package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.RandomPetApiService;
import io.swagger.api.factories.RandomPetApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.SinglePetResponse;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;


@Path("/randomPet")


public class RandomPetApi  {
   private final RandomPetApiService delegate;

   public RandomPetApi(@Context ServletConfig servletContext) {
      RandomPetApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("RandomPetApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (RandomPetApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = RandomPetApiServiceFactory.getRandomPetApi();
      }

      this.delegate = delegate;
   }

    @GET
    
    
    @Produces({ "application/json" })
    @Operation(summary = "", description = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "a single random pet", content = @Content(schema = @Schema(implementation = SinglePetResponse.class))) })
    public Response getRandomPet(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getRandomPet(securityContext);
    }
}
