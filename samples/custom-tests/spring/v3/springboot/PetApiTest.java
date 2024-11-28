package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

@RunWith(SpringRunner.class)
@SpringBootTest
public class PetApiTest {

    @Autowired
    private PetApi api;

    @Test
    public void addPetTest() throws Exception {
        Pet body = new Pet()
                .id(789L)
                .name("doggie")
                .status(Pet.StatusEnum.AVAILABLE);
        ResponseEntity<Pet> responseEntity = api.addPet(body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void deletePetTest() throws Exception {
        Long petId = 789L;
        String apiKey = "apiKey_example";
        ResponseEntity<Void> responseEntity = api.deletePet(petId, apiKey);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void findPetsByStatusTest() throws Exception {
        String status = "available";
        ResponseEntity<List<Pet>> responseEntity = api.findPetsByStatus(status);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void findPetsByTagsTest() throws Exception {
        List<String> tags = Arrays.asList("tags_example");
        ResponseEntity<List<Pet>> responseEntity = api.findPetsByTags(tags);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void getPetByIdTest() throws Exception {
        Long petId = 789L;
        ResponseEntity<Pet> responseEntity = api.getPetById(petId);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void updatePetTest() throws Exception {
        Pet body = new Pet()
                .id(789L)
                .name("doggie")
                .status(Pet.StatusEnum.AVAILABLE);
        ResponseEntity<Pet> responseEntity = api.updatePet(body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

    @Test
    public void uploadFileTest() throws Exception {
        Long petId = 789L;
        String additionalMetadata = "additionalMetadata_example";
        org.springframework.web.multipart.MultipartFile body = null;
        ResponseEntity<ModelApiResponse> responseEntity = api.uploadFile(petId, additionalMetadata, body);
        assertEquals(HttpStatus.NOT_IMPLEMENTED, responseEntity.getStatusCode());
    }

}
