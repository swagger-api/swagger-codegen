package io.swagger.api;

import io.swagger.model.Animal;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/")
public interface AnimalApi  {

    /**
     * Add a new animal to the store
     *
     */
    @POST
    @Path("/animal")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new animal to the store", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void addAnimal(@Valid Animal body);

    /**
     * Deletes a animal
     *
     */
    @DELETE
    @Path("/animal/{animalId}")
    @Operation(summary = "Deletes a animal", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid animal value") })
    public void deleteAnimal(@PathParam("animalId") Long animalId, @HeaderParam("api_key") String apiKey);

    /**
     * Find animal by ID
     *
     * Returns a single animal
     *
     */
    @GET
    @Path("/animal/{animalId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find animal by ID", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Animal.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Animal getAnimalById(@PathParam("animalId") Long animalId);

    /**
     * Update an existing animal
     *
     */
    @PUT
    @Path("/animal")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing animal", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Animal not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public void updateAnimal(@Valid Animal body);

    /**
     * Updates a animal
     *
     */
    @POST
    @Path("/animal/{animalId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a animal", tags={ "animal" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void updateAnimalWithForm(@PathParam("animalId") Long animalId, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status);
}
