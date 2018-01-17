package io.swagger.client.api;

import java.io.File;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import io.restassured.RestAssured;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.response.Response;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import static io.restassured.http.Method.*;
import static io.restassured.mapper.ObjectMapperType.GSON;

public class PetApi {

    private RequestSpecBuilder reqSpec;

    private PetApi(RequestSpecBuilder reqSpec) {
        this.reqSpec = reqSpec;
    }

    public static PetApi pet(RequestSpecBuilder reqSpec) {
        return new PetApi(reqSpec);
    }

    public AddPetOper addPet() {
        return new AddPetOper(reqSpec);
    }

    public DeletePetOper deletePet() {
        return new DeletePetOper(reqSpec);
    }

    public FindPetsByStatusOper findPetsByStatus() {
        return new FindPetsByStatusOper(reqSpec);
    }

    @Deprecated
    public FindPetsByTagsOper findPetsByTags() {
        return new FindPetsByTagsOper(reqSpec);
    }

    public GetPetByIdOper getPetById() {
        return new GetPetByIdOper(reqSpec);
    }

    public UpdatePetOper updatePet() {
        return new UpdatePetOper(reqSpec);
    }

    public UpdatePetWithFormOper updatePetWithForm() {
        return new UpdatePetWithFormOper(reqSpec);
    }

    public UploadFileOper uploadFile() {
        return new UploadFileOper(reqSpec);
    }


    /**
     * Add a new pet to the store
     * 
     *
     * @see #body Pet object that needs to be added to the store (required)
     */
    public class AddPetOper {

        public static final String REQ_URI = "/pet";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public AddPetOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public AddPetOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /pet
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }


         /**
         * @param body Pet object that needs to be added to the store (required)
         */
        public AddPetOper body(Pet body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public AddPetOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public AddPetOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Deletes a pet
     * 
     *
     * @see #petIdPath Pet id to delete (required)
     * @see #apiKeyHeader  (optional)
     */
    public class DeletePetOper {

        public static final String REQ_URI = "/pet/{petId}";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public DeletePetOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public DeletePetOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * DELETE /pet/{petId}
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(DELETE, REQ_URI));
        }



        /**
         * @param apiKey  (optional)
         */
        public DeletePetOper apiKeyHeader(String apiKey) {
            reqSpec.addHeader("api_key", apiKey);
            return this;
        }

        /**
         * @param petId Pet id to delete (required)
         */
        public DeletePetOper petIdPath(Long petId) {
            reqSpec.addPathParam("petId", petId);
            return this;
        }



        /**
         * Customise request specification
         */
        public DeletePetOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public DeletePetOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     *
     * @see #statusQuery Status values that need to be considered for filter (required)
     * return List<Pet>
     */
    public class FindPetsByStatusOper {

        public static final String REQ_URI = "/pet/findByStatus";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public FindPetsByStatusOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public FindPetsByStatusOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * GET /pet/findByStatus
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(GET, REQ_URI));
        }


        /**
         * GET /pet/findByStatus
         * @return List<Pet>
         */
        public List<Pet> executeAs(Function<Response, Response> handler) {
            return Arrays.asList(execute(handler).as(Pet[].class, GSON));
        }




        /**
         * @param status Status values that need to be considered for filter (required)
         */
        public FindPetsByStatusOper statusQuery(List<String> status) {
            reqSpec.addQueryParam("status", status);
            return this;
        }


        /**
         * Customise request specification
         */
        public FindPetsByStatusOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public FindPetsByStatusOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * @see #tagsQuery Tags to filter by (required)
     * return List<Pet>
     * @deprecated
     */
    @Deprecated
    public class FindPetsByTagsOper {

        public static final String REQ_URI = "/pet/findByTags";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public FindPetsByTagsOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public FindPetsByTagsOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * GET /pet/findByTags
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(GET, REQ_URI));
        }


        /**
         * GET /pet/findByTags
         * @return List<Pet>
         */
        public List<Pet> executeAs(Function<Response, Response> handler) {
            return Arrays.asList(execute(handler).as(Pet[].class, GSON));
        }




        /**
         * @param tags Tags to filter by (required)
         */
        public FindPetsByTagsOper tagsQuery(List<String> tags) {
            reqSpec.addQueryParam("tags", tags);
            return this;
        }


        /**
         * Customise request specification
         */
        public FindPetsByTagsOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public FindPetsByTagsOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Find pet by ID
     * Returns a single pet
     *
     * @see #petIdPath ID of pet to return (required)
     * return Pet
     */
    public class GetPetByIdOper {

        public static final String REQ_URI = "/pet/{petId}";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public GetPetByIdOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public GetPetByIdOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * GET /pet/{petId}
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(GET, REQ_URI));
        }

        /**
         * GET /pet/{petId}
         * @return Pet
         */
        public Pet executeAs(Function<Response, Response> handler) {
            return execute(handler).as(Pet.class, GSON);
        }




        /**
         * @param petId ID of pet to return (required)
         */
        public GetPetByIdOper petIdPath(Long petId) {
            reqSpec.addPathParam("petId", petId);
            return this;
        }



        /**
         * Customise request specification
         */
        public GetPetByIdOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public GetPetByIdOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Update an existing pet
     * 
     *
     * @see #body Pet object that needs to be added to the store (required)
     */
    public class UpdatePetOper {

        public static final String REQ_URI = "/pet";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public UpdatePetOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public UpdatePetOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * PUT /pet
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(PUT, REQ_URI));
        }


         /**
         * @param body Pet object that needs to be added to the store (required)
         */
        public UpdatePetOper body(Pet body) {
            reqSpec.setBody(body, GSON);
            return this;
        }





        /**
         * Customise request specification
         */
        public UpdatePetOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public UpdatePetOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * Updates a pet in the store with form data
     * 
     *
     * @see #petIdPath ID of pet that needs to be updated (required)
     * @see #nameForm Updated name of the pet (optional)
     * @see #statusForm Updated status of the pet (optional)
     */
    public class UpdatePetWithFormOper {

        public static final String REQ_URI = "/pet/{petId}";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public UpdatePetWithFormOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public UpdatePetWithFormOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /pet/{petId}
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }




        /**
         * @param petId ID of pet that needs to be updated (required)
         */
        public UpdatePetWithFormOper petIdPath(Long petId) {
            reqSpec.addPathParam("petId", petId);
            return this;
        }


         /**
         * @param name Updated name of the pet (optional)
         */
         public UpdatePetWithFormOper nameForm(String name) {
            reqSpec.addFormParam("name", name);
            return this;
         }
         /**
         * @param status Updated status of the pet (optional)
         */
         public UpdatePetWithFormOper statusForm(String status) {
            reqSpec.addFormParam("status", status);
            return this;
         }

        /**
         * Customise request specification
         */
        public UpdatePetWithFormOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public UpdatePetWithFormOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
    /**
     * uploads an image
     * 
     *
     * @see #petIdPath ID of pet to update (required)
     * @see #additionalMetadataForm Additional data to pass to server (optional)
     * @see #fileForm file to upload (optional)
     * return ModelApiResponse
     */
    public class UploadFileOper {

        public static final String REQ_URI = "/pet/{petId}/uploadImage";

        private RequestSpecBuilder reqSpec;

        private ResponseSpecBuilder respSpec;

        public UploadFileOper() {
            this.reqSpec = new RequestSpecBuilder();
            this.respSpec = new ResponseSpecBuilder();
        }

        public UploadFileOper(RequestSpecBuilder reqSpec) {
            this.reqSpec = reqSpec;
            this.respSpec = new ResponseSpecBuilder();
        }

        /**
         * POST /pet/{petId}/uploadImage
         */
        public <T> T execute(Function<Response, T> handler) {
            return handler.apply(RestAssured.given().spec(reqSpec.build()).expect().spec(respSpec.build()).when().request(POST, REQ_URI));
        }

        /**
         * POST /pet/{petId}/uploadImage
         * @return ModelApiResponse
         */
        public ModelApiResponse executeAs(Function<Response, Response> handler) {
            return execute(handler).as(ModelApiResponse.class, GSON);
        }




        /**
         * @param petId ID of pet to update (required)
         */
        public UploadFileOper petIdPath(Long petId) {
            reqSpec.addPathParam("petId", petId);
            return this;
        }


         /**
         * @param additionalMetadata Additional data to pass to server (optional)
         */
         public UploadFileOper additionalMetadataForm(String additionalMetadata) {
            reqSpec.addFormParam("additionalMetadata", additionalMetadata);
            return this;
         }
         /**
         * @param file file to upload (optional)
         */
         public UploadFileOper fileForm(File file) {
            reqSpec.addFormParam("file", file);
            return this;
         }

        /**
         * Customise request specification
         */
        public UploadFileOper reqSpec(Consumer<RequestSpecBuilder> consumer) {
            consumer.accept(reqSpec);
            return this;
        }

        /**
         * Customise response specification
         */
        public UploadFileOper respSpec(Consumer<ResponseSpecBuilder> consumer) {
            consumer.accept(respSpec);
            return this;
        }
    }
}