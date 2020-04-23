package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.AnotherFakeApiService;
import io.swagger.api.factories.AnotherFakeApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import io.swagger.model.Client;

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

@Path("/another-fake/dummy")



public class AnotherFakeApi  {
   private final AnotherFakeApiService delegate;

   public AnotherFakeApi(@Context ServletConfig servletContext) {
      AnotherFakeApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("AnotherFakeApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (AnotherFakeApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = AnotherFakeApiServiceFactory.getAnotherFakeApi();
      }

      this.delegate = delegate;
   }

    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test special tags", description = "To test special tags", tags={ "$another-fake?" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class))) })
    public Response testSpecialTags(@Parameter(in = ParameterIn.DEFAULT, description = "client model" ,required=true) Client body

,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testSpecialTags(body,securityContext);
    }
}
