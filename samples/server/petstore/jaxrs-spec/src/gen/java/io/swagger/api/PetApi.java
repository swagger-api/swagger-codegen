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

@javax.annotation.Generated(value = "io.swagger.codegen.languages.java.JavaJAXRSSpecServerCodegen", date = "2018-04-04T20:14:22.579+02:00[Europe/Zurich]")
public class PetApi {



    @POST
    @ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class)
    })
    public Response addPet(@Valid Pet pet) {
        return Response.ok().entity("magic!").build();
    }


    @DELETE
    @Path("/{petId}")
    @ApiOperation(value = "Deletes a pet", notes = "", response = Void.class, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid pet value", response = Void.class)
    })
    public Response deletePet(@PathParam("petId") @ApiParam("Pet id to delete") Integer petId,@HeaderParam("api_key")   String apiKey) {
        return Response.ok().entity("magic!").build();
    }


    @GET
    @Path("/findByStatus")
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", response = Void.class)
    })
    public Response findPetsByStatus(@QueryParam("status") @NotNull   @ApiParam("Status values that need to be considered for filter")  List<String> status) {
        return Response.ok().entity("magic!").build();
    }


    @GET
    @Path("/findByTags")
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid tag value", response = Void.class)
    })
    public Response findPetsByTags(@QueryParam("tags") @NotNull   @ApiParam("Tags to filter by")  List<String> tags) {
        return Response.ok().entity("magic!").build();
    }


    @GET
    @Path("/{petId}")
    @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class)
    })
    public Response getPetById(@PathParam("petId") @ApiParam("ID of pet to return") Integer petId) {
        return Response.ok().entity("magic!").build();
    }


    @PUT
    @ApiOperation(value = "Update an existing pet", notes = "", response = Void.class, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class),
        @ApiResponse(code = 405, message = "Validation exception", response = Void.class)
    })
    public Response updatePet(@Valid Pet pet) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @Path("/{petId}")
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class)
    })
    public Response updatePetWithForm(@PathParam("petId") @ApiParam("ID of pet that needs to be updated") Integer petId,@Valid Object body) {
        return Response.ok().entity("magic!").build();
    }


    @POST
    @Path("/{petId}/uploadImage")
    @ApiOperation(value = "uploads an image", notes = "", response = ModelApiResponse.class, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class)
    })
    public Response uploadFile(@PathParam("petId") @ApiParam("ID of pet to update") Integer petId,@Valid Object body) {
        return Response.ok().entity("magic!").build();
    }

}
