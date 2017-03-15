package io.swagger.api.impl;

import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import io.swagger.api.PetApi;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;

import java.util.List;

@Path("/pet")

@Api(description = "the pet API")




public class PetApiServiceImpl implements PetApi {

    public Response addPet(Pet body) {
        return Response.ok().entity("magic!").build();
    }

    public Response deletePet(Long petId, String apiKey) {
        return Response.ok().entity("magic!").build();
    }

    public Response findPetsByStatus(List<String> status) {
        return Response.ok().entity("magic!").build();
    }

    public Response findPetsByTags(List<String> tags) {
        return Response.ok().entity("magic!").build();
    }

    public Response getPetById(Long petId) {
        return Response.ok().entity("magic!").build();
    }

    public Response updatePet(Pet body) {
        return Response.ok().entity("magic!").build();
    }

    public Response updatePetWithForm(Long petId, String name, String status) {
        return Response.ok().entity("magic!").build();
    }

    public Response uploadFile(Long petId, String additionalMetadata, InputStream fileDetail) {
        return Response.ok().entity("magic!").build();
    }
}

