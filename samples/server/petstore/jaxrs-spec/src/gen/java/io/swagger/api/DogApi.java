package io.swagger.api;

import io.swagger.model.Dog;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/dog")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2022-07-18T07:27:01.025-05:00[America/Bogota]")
public class DogApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new dog to the store", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input")
    })
    public Response addDog(@Valid Dog body) {
        return Response.ok().entity("magic!").build();
    }
    @DELETE
    @Path("/{dogId}")
    @Operation(summary = "Deletes a dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid dog value")
    })
    public Response deleteDog( @PathParam("dogId")

 @Parameter(description = "Dog id to delete") Long dogId
,  @HeaderParam("api_key") 

 String apiKey
) {
        return Response.ok().entity("magic!").build();
    }
    @GET
    @Path("/{dogId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find dog by ID", description = "Returns a single dog", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/xml", schema = @Schema(implementation = Dog.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found")
    })
    public Response getDogById( @PathParam("dogId")

 @Parameter(description = "ID of dog to return") Long dogId
) {
        return Response.ok().entity("magic!").build();
    }
    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Animal not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception")
    })
    public Response updateDog(@Valid Dog body) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Path("/{dogId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a dog", description = "", tags={ "dog" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input")
    })
    public Response updateDogWithForm( @PathParam("dogId")

 @Parameter(description = "ID of dog that needs to be updated") Long dogId
,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status) {
        return Response.ok().entity("magic!").build();
    }}
