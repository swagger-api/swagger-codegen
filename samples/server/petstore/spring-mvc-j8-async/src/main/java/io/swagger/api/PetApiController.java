package io.swagger.api;

import org.springframework.stereotype.Controller;



@Controller
public class PetApiController implements PetApi {



    public Callable<ResponseEntity<Void>> addPet(@ApiParam(value = "Pet object that needs to be added to the store" ,required=true ) @RequestBody Pet body) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> deletePet(@ApiParam(value = "Pet id to delete",required=true ) @PathVariable("petId") Long petId,
        @ApiParam(value = ""  ) @RequestHeader(value="api_key", required=false) String apiKey) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<List<Pet>>> findPetsByStatus(@ApiParam(value = "Status values that need to be considered for filter", required = true, allowableValues = "AVAILABLE, PENDING, SOLD") @RequestParam(value = "status", required = true) List<String> status) {
        // do some magic!
        return new Callable<ResponseEntity<List<Pet>>>() {
            @Override
            public ResponseEntity<List<Pet>> call() throws Exception {
                return new ResponseEntity<List<Pet>>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<List<Pet>>> findPetsByTags(@ApiParam(value = "Tags to filter by", required = true) @RequestParam(value = "tags", required = true) List<String> tags) {
        // do some magic!
        return new Callable<ResponseEntity<List<Pet>>>() {
            @Override
            public ResponseEntity<List<Pet>> call() throws Exception {
                return new ResponseEntity<List<Pet>>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Pet>> getPetById(@ApiParam(value = "ID of pet to return",required=true ) @PathVariable("petId") Long petId) {
        // do some magic!
        return new Callable<ResponseEntity<Pet>>() {
            @Override
            public ResponseEntity<Pet> call() throws Exception {
                return new ResponseEntity<Pet>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> updatePet(@ApiParam(value = "Pet object that needs to be added to the store" ,required=true ) @RequestBody Pet body) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> updatePetWithForm(@ApiParam(value = "ID of pet that needs to be updated",required=true ) @PathVariable("petId") Long petId,
        @ApiParam(value = "Updated name of the pet" ) @RequestPart(value="name", required=false)  String name,
        @ApiParam(value = "Updated status of the pet" ) @RequestPart(value="status", required=false)  String status) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<ModelApiResponse>> uploadFile(@ApiParam(value = "ID of pet to update",required=true ) @PathVariable("petId") Long petId,
        @ApiParam(value = "Additional data to pass to server" ) @RequestPart(value="additionalMetadata", required=false)  String additionalMetadata,
        @ApiParam(value = "file detail") @RequestPart("file") MultipartFile file) {
        // do some magic!
        return new Callable<ResponseEntity<ModelApiResponse>>() {
            @Override
            public ResponseEntity<ModelApiResponse> call() throws Exception {
                return new ResponseEntity<ModelApiResponse>(HttpStatus.OK);
            }
        };
    }

}
