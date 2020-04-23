package io.swagger.api;

import io.swagger.model.Animal;

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

@Path("/animal")

@javax.annotation.Generated(value = "io.swagger.codegen.v3.generators.java.JavaJAXRSSpecServerCodegen", date = "2020-03-13T07:31:34.535-05:00[America/Bogota]")
public class AnimalApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new animal to the store", description = "", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input")
    })
    public Response addAnimal(@Valid Animal body) {
        return Response.ok().entity("magic!").build();
    }
    @DELETE
    @Path("/{animalId}")
    @Operation(summary = "Deletes a animal", description = "", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid animal value")
    })
    public Response deleteAnimal( @PathParam("animalId")

 @Parameter(description = "Animal id to delete") Long animalId
,  @HeaderParam("api_key") 

 String apiKey
) {
        return Response.ok().entity("magic!").build();
    }
    @GET
    @Path("/{animalId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find animal by ID", description = "Returns a single animal", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Animal.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found")
    })
    public Response getAnimalById( @PathParam("animalId")

 @Parameter(description = "ID of pet to return") Long animalId
) {
        return Response.ok().entity("magic!").build();
    }
    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing animal", description = "", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Animal not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception")
    })
    public Response updateAnimal(@Valid Animal body) {
        return Response.ok().entity("magic!").build();
    }
    @POST
    @Path("/{animalId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a animal", description = "", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input")
    })
    public Response updateAnimalWithForm( @PathParam("animalId")

 @Parameter(description = "ID of animal that needs to be updated") Long animalId
,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status) {
        return Response.ok().entity("magic!").build();
    }}
