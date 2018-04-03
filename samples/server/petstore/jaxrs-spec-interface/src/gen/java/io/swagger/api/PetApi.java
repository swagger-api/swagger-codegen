package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;


import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/pet")
@Api(description = "the pet API")

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-03-15T06:28:22.549+01:00[Europe/Zurich]")
public interface PetApi {



    @POST
    @ApiOperation(value = "Add a new pet to the store", notes = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    void addPet(@Valid Pet pet);


    @DELETE
    @Path("/{petId}")
    @ApiOperation(value = "Deletes a pet", notes = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid pet value", response = Void.class) })
    void deletePet(@PathParam("petId") @ApiParam("Pet id to delete") Integer petId,@HeaderParam("api_key")   String apiKey);


    @GET
    @Path("/findByStatus")
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", response = Void.class, responseContainer = "List") })
    List<Pet> findPetsByStatus(@QueryParam("status") @NotNull   @ApiParam("Status values that need to be considered for filter")  List<String> status);


    @GET
    @Path("/findByTags")
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid tag value", response = Void.class, responseContainer = "List") })
    List<Pet> findPetsByTags(@QueryParam("tags") @NotNull   @ApiParam("Tags to filter by")  List<String> tags);


    @GET
    @Path("/{petId}")
    @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    Pet getPetById(@PathParam("petId") @ApiParam("ID of pet to return") Integer petId);


    @PUT
    @ApiOperation(value = "Update an existing pet", notes = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class),
        @ApiResponse(code = 405, message = "Validation exception", response = Void.class) })
    void updatePet(@Valid Pet pet);


    @POST
    @Path("/{petId}")
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    void updatePetWithForm(@PathParam("petId") @ApiParam("ID of pet that needs to be updated") Integer petId,@Valid Object body);


    @POST
    @Path("/{petId}/uploadImage")
    @ApiOperation(value = "uploads an image", notes = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    ModelApiResponse uploadFile(@PathParam("petId") @ApiParam("ID of pet to update") Integer petId,@Valid Object body);

}
