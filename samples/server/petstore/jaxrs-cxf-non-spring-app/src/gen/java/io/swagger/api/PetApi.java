package io.swagger.api;

import io.swagger.model.AllPetsResponse;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import io.swagger.model.SinglePetResponse;
import io.swagger.model.SubCategory;

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
 * <p>This is a sample Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/). 
 *
 */
@Path("/")
public interface PetApi  {

    /**
     * Add a new pet to the store
     *
     */
    @POST
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new pet to the store", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void addPet(@Valid Pet body);

    /**
     * Deletes a pet
     *
     */
    @DELETE
    @Path("/pet/{petId}")
    @Operation(summary = "Deletes a pet", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public void deletePet(@PathParam("petId") Long petId, @HeaderParam("api_key") String apiKey);

    @POST
    @Path("/pet/category")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public ModelApiResponse doCategoryStuff(@Valid SubCategory body);

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     */
    @GET
    @Path("/pet/findByStatus")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by status", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    public List<Pet> findPetsByStatus(@QueryParam("status") @NotNull List<String> status);

    /**
     * Finds Pets by tags
     *
     * Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.
     *
     */
    @GET
    @Path("/pet/findByTags")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Finds Pets by tags", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    public List<Pet> findPetsByTags(@QueryParam("tags") @NotNull List<String> tags);

    @GET
    @Path("/allPets")
    @Produces({ "application/json" })
    @Operation(summary = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "a single random pet", content = @Content(schema = @Schema(implementation = AllPetsResponse.class))) })
    public AllPetsResponse getAllPets();

    /**
     * Find pet by ID
     *
     * Returns a single pet
     *
     */
    @GET
    @Path("/pet/{petId}")
    @Produces({ "application/json", "application/xml" })
    @Operation(summary = "Find pet by ID", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Pet getPetById(@PathParam("petId") Long petId);

    @GET
    @Path("/randomPet")
    @Produces({ "application/json" })
    @Operation(summary = "", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "a single random pet", content = @Content(schema = @Schema(implementation = SinglePetResponse.class))) })
    public SinglePetResponse getRandomPet();

    /**
     * Update an existing pet
     *
     */
    @PUT
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing pet", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public void updatePet(@Valid Pet body);

    /**
     * Updates a pet in the store with form data
     *
     */
    @POST
    @Path("/pet/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a pet in the store with form data", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void updatePetWithForm(@PathParam("petId") Long petId, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status);

    /**
     * uploads an image
     *
     */
    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes({ "application/octet-stream" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public ModelApiResponse uploadFile(@PathParam("petId") Long petId, @Valid Object body);
}
