package io.swagger.client.api;

import io.swagger.TestUtils;

import io.swagger.client.*;
import io.swagger.client.auth.*;
import io.swagger.client.model.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;

public class PetApiTest {
    PetApi api = null;

    @Before
    public void setup() {
        api = new PetApi();
        // setup authentication
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) api.getApiClient().getAuthentication("api_key");
        apiKeyAuth.setApiKey("special-key");
    }

    @Test
    public void testApiClient() {
        // the default api client is used
        assertEquals(Configuration.getDefaultApiClient(), api.getApiClient());
        assertNotNull(api.getApiClient());
        assertEquals("http://petstore.swagger.io:80/v2", api.getApiClient().getBasePath());
        assertFalse(api.getApiClient().isDebugging());

        ApiClient oldClient = api.getApiClient();

        ApiClient newClient = new ApiClient();
        newClient.setBasePath("http://example.com");
        newClient.setDebugging(true);

        // set api client via constructor
        api = new PetApi(newClient);
        assertNotNull(api.getApiClient());
        assertEquals("http://example.com", api.getApiClient().getBasePath());
        assertTrue(api.getApiClient().isDebugging());

        // set api client via setter method
        api.setApiClient(oldClient);
        assertNotNull(api.getApiClient());
        assertEquals("http://petstore.swagger.io:80/v2", api.getApiClient().getBasePath());
        assertFalse(api.getApiClient().isDebugging());
    }

    @Test
    public void testCreateAndGetPet() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertPetMatches(pet, fetched);
    }

    /*
    @Test
    public void testCreateAndGetPetWithByteArray() throws Exception {
        Pet pet = createRandomPet();
        byte[] bytes = serializeJson(pet, api.getApiClient()).getBytes();
        api.addPetUsingByteArray(bytes);

        byte[] fetchedBytes = api.petPetIdtestingByteArraytrueGet(pet.getId());
        Type type = new TypeToken<Pet>(){}.getType();
        Pet fetched = deserializeJson(new String(fetchedBytes), type, api.getApiClient());
        assertPetMatches(pet, fetched);
    }
    */

    @Test
    public void testCreateAndGetPetWithHttpInfo() throws Exception {
        Pet pet = createRandomPet();
        api.addPetWithHttpInfo(pet);

        ApiResponse<Pet> resp = api.getPetByIdWithHttpInfo(pet.getId());
        assertEquals(200, resp.getStatusCode());
        assertEquals("application/json", resp.getHeaders().get("Content-Type").get(0));
        Pet fetched = resp.getData();
        assertPetMatches(pet, fetched);
    }

    @Test
    public void testCreateAndGetPetAsync() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        final CountDownLatch latch1 = new CountDownLatch(1);
        final TestApiCallback<Pet> callback1 = new TestApiCallback<Pet>(latch1);

        api.getPetByIdAsync(pet.getId(), callback1);
        // the API call should be executed asynchronously, so callback success should be null at the moment
        assertFalse(callback1.isDone());

        // wait for the asynchronous call to finish (at most 10 seconds)
        assertTrue("have not got result of getPetByIdAsync after 10 seconds",
                   latch1.await(10, TimeUnit.SECONDS));

        assertTrue(callback1.isDone());
        if (!callback1.isSuccess()) fail(callback1.getException().getMessage());
        Pet fetched = callback1.getResult();
        assertPetMatches(pet, fetched);

        // test getting a nonexistent pet
        final CountDownLatch latch2 = new CountDownLatch(1);
        final TestApiCallback<Pet> callback2 = new TestApiCallback<Pet>(latch2);
        api.getPetByIdAsync(-10000L, callback2);

        // wait for the asynchronous call to finish (at most 10 seconds)
        assertTrue("have not got result of getPetByIdAsync after 10 seconds",
                   latch2.await(10, TimeUnit.SECONDS));

        assertTrue(callback2.isDone());
        assertFalse("expected an error", callback2.isSuccess());
        ApiException exception = callback2.getException();
        assertNotNull(exception);
        assertEquals(404, exception.getCode());
        assertEquals("Not Found", exception.getMessage());
        assertEquals("application/json", exception.getResponseHeaders().get("Content-Type").get(0));
    }

    @Test
    public void testCreateAndGetMultiplePetsAsync() throws Exception {
        Pet pet1 = createRandomPet();
        Pet pet2 = createRandomPet();

        final CountDownLatch addLatch = new CountDownLatch(2);
        final TestApiCallback<Void> addCallback1 = new TestApiCallback<Void>(addLatch);
        final TestApiCallback<Void> addCallback2 = new TestApiCallback<Void>(addLatch);

        // Make 2 simultaneous calls
        api.addPetAsync(pet1, addCallback1);
        api.addPetAsync(pet2, addCallback2);

        // wait for both asynchronous calls to finish (at most 10 seconds)
        assertTrue(addLatch.await(10, TimeUnit.SECONDS));

        assertTrue(addCallback1.isDone());
        assertTrue(addCallback2.isDone());

        if (!addCallback1.isSuccess()) throw addCallback1.getException();
        if (!addCallback2.isSuccess()) throw addCallback2.getException();

        assertValidProgress(addCallback1.getUploadProgress());
        assertValidProgress(addCallback2.getUploadProgress());

        final CountDownLatch getLatch = new CountDownLatch(3);
        final TestApiCallback<Pet> getCallback1 = new TestApiCallback<Pet>(getLatch);
        final TestApiCallback<Pet> getCallback2 = new TestApiCallback<Pet>(getLatch);
        final TestApiCallback<Pet> getCallback3 = new TestApiCallback<Pet>(getLatch);

        api.getPetByIdAsync(pet1.getId(), getCallback1);
        api.getPetByIdAsync(pet2.getId(), getCallback2);
        // Get nonexistent pet
        api.getPetByIdAsync(-10000L, getCallback3);

        // wait for all asynchronous calls to finish (at most 10 seconds)
        assertTrue(getLatch.await(10, TimeUnit.SECONDS));

        assertTrue(getCallback1.isDone());
        assertTrue(getCallback2.isDone());
        assertTrue(getCallback3.isDone());

        if (!getCallback1.isSuccess()) throw getCallback1.getException();
        if (!getCallback2.isSuccess()) throw getCallback2.getException();

        assertPetMatches(pet1, getCallback1.getResult());
        assertPetMatches(pet2, getCallback2.getResult());

        assertValidProgress(getCallback1.getDownloadProgress());
        assertValidProgress(getCallback2.getDownloadProgress());

        // Last callback should fail with ApiException
        assertFalse(getCallback3.isSuccess());
        final ApiException exception = getCallback3.getException();
        assertNotNull(exception);
        assertEquals(404, exception.getCode());
    }

    /*
    @Test
    public void testGetPetByIdInObject() throws Exception {
        Pet pet = new Pet();
        pet.setId(TestUtils.nextId());
        pet.setName("pet " + pet.getId());

        Category category = new Category();
        category.setId(TestUtils.nextId());
        category.setName("category " + category.getId());
        pet.setCategory(category);

        pet.setStatus(Pet.StatusEnum.PENDING);
        List<String> photos = Arrays.asList(new String[]{"http://foo.bar.com/1"});
        pet.setPhotoUrls(photos);

        api.addPet(pet);

        InlineResponse200 fetched = api.getPetByIdInObject(pet.getId());
        assertEquals(pet.getId(), fetched.getId());
        assertEquals(pet.getName(), fetched.getName());

        Object categoryObj = fetched.getCategory();
        assertNotNull(categoryObj);
        assertTrue(categoryObj instanceof Map);

        Map categoryMap = (Map) categoryObj;
        Object categoryIdObj = categoryMap.get("id");
        // NOTE: Gson parses integer value to double.
        assertTrue(categoryIdObj instanceof Double);
        Long categoryIdLong = ((Double) categoryIdObj).longValue();
        assertEquals(category.getId(), categoryIdLong);
        assertEquals(category.getName(), categoryMap.get("name"));
    }
    */

    @Test
    public void testUpdatePet() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");

        api.updatePet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertPetMatches(pet, fetched);
    }

    @Test
    public void testFindPetsByStatus() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.PENDING);

        api.updatePet(pet);

        List<Pet> pets = api.findPetsByStatus(Arrays.asList("pending"));
        assertNotNull(pets);

        boolean found = false;
        for (Pet fetched : pets) {
            if (fetched.getId().equals(pet.getId())) {
                found = true;
                break;
            }
        }

        assertTrue(found);

        api.deletePet(pet.getId(), null);
    }

    @Test
    @Ignore
    public void testFindPetsByTags() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("monster");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        List<Tag> tags = new ArrayList<Tag>();
        Tag tag1 = new Tag();
        tag1.setName("friendly");
        tags.add(tag1);
        pet.setTags(tags);

        api.updatePet(pet);

        List<Pet> pets = api.findPetsByTags(Arrays.asList("friendly"));
        assertNotNull(pets);

        boolean found = false;
        for (Pet fetched : pets) {
            if (fetched.getId().equals(pet.getId())) {
                found = true;
                break;
            }
        }
        assertTrue(found);

        api.deletePet(pet.getId(), null);
    }

    @Test
    public void testUpdatePetWithForm() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("frank");
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());

        api.updatePetWithForm(fetched.getId(), "furt", null);
        Pet updated = api.getPetById(fetched.getId());

        assertEquals(updated.getName(), "furt");
    }

    @Test
    public void testDeletePet() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        api.deletePet(fetched.getId(), null);

        try {
            fetched = api.getPetById(fetched.getId());
            fail("expected an error");
        } catch (ApiException e) {
            assertEquals(404, e.getCode());
        }
    }

    @Test
    public void testUploadFile() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        File file = new File("hello.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));
        writer.write("Hello world!");
        writer.close();

        api.uploadFile(pet.getId(), "a test file", new File(file.getAbsolutePath()));
    }

    @Test
    public void testEqualsAndHashCode() {
        Pet pet1 = new Pet();
        Pet pet2 = new Pet();
        assertTrue(pet1.equals(pet2));
        assertTrue(pet2.equals(pet1));
        assertTrue(pet1.hashCode() == pet2.hashCode());
        assertTrue(pet1.equals(pet1));
        assertTrue(pet1.hashCode() == pet1.hashCode());

        pet2.setName("really-happy");
        pet2.setPhotoUrls(Arrays.asList("http://foo.bar.com/1", "http://foo.bar.com/2"));
        assertFalse(pet1.equals(pet2));
        assertFalse(pet2.equals(pet1));
        assertFalse(pet1.hashCode() == (pet2.hashCode()));
        assertTrue(pet2.equals(pet2));
        assertTrue(pet2.hashCode() == pet2.hashCode());

        pet1.setName("really-happy");
        pet1.setPhotoUrls(Arrays.asList("http://foo.bar.com/1", "http://foo.bar.com/2"));
        assertTrue(pet1.equals(pet2));
        assertTrue(pet2.equals(pet1));
        assertTrue(pet1.hashCode() == pet2.hashCode());
        assertTrue(pet1.equals(pet1));
        assertTrue(pet1.hashCode() == pet1.hashCode());
    }

    private Pet createRandomPet() {
        Pet pet = new Pet();
        pet.setId(TestUtils.nextId());
        pet.setName("gorilla");

        Category category = new Category();
        category.setName("really-happy");

        pet.setCategory(category);
        pet.setStatus(Pet.StatusEnum.AVAILABLE);
        List<String> photos = Arrays.asList("http://foo.bar.com/1", "http://foo.bar.com/2");
        pet.setPhotoUrls(photos);

        return pet;
    }

    private String serializeJson(Object o, ApiClient apiClient) {
        return apiClient.getJSON().serialize(o);
    }

    private <T> T deserializeJson(String json, Type type, ApiClient apiClient) {
        return (T) apiClient.getJSON().deserialize(json, type);
    }

    private void assertPetMatches(Pet expected, Pet actual) {
        assertNotNull(actual);
        assertEquals(expected.getId(), actual.getId());
        assertNotNull(actual.getCategory());
        assertEquals(expected.getCategory().getName(),
                     actual.getCategory().getName());
    }

    /**
     * Assert that the given upload/download progress list satisfies the
     * following constraints:
     *
     *     - List is not empty
     *     - Byte count should be nondecreasing
     *     - The last element, and only the last element, should have done=true
     */
    private void assertValidProgress(List<Progress> progressList) {
        assertFalse(progressList.isEmpty());

        Progress prev = null;
        int index = 0;
        for (Progress progress : progressList) {
            if (prev != null) {
                if (prev.done || prev.bytes > progress.bytes) {
                    fail("Progress list out of order at index " + index
                         + ": " + progressList);
                }
            }
            prev = progress;
            index += 1;
        }

        if (!prev.done) {
            fail("Last progress item should have done=true: " + progressList);
        }
    }

    private static class TestApiCallback<T> implements ApiCallback<T> {

        private final CountDownLatch latch;
        private final ConcurrentLinkedQueue<Progress> uploadProgress =
            new ConcurrentLinkedQueue<Progress>();
        private final ConcurrentLinkedQueue<Progress> downloadProgress =
            new ConcurrentLinkedQueue<Progress>();

        private boolean done;
        private boolean success;
        private ApiException exception;
        private T result;

        public TestApiCallback(CountDownLatch latch) {
            this.latch = latch;
            this.done = false;
        }

        @Override
        public void onFailure(ApiException e, int statusCode, Map<String, List<String>> responseHeaders) {
            exception = e;
            this.done = true;
            this.success = false;
            latch.countDown();
        }

        @Override
        public void onSuccess(T result, int statusCode, Map<String, List<String>> responseHeaders) {
            this.result = result;
            this.done = true;
            this.success = true;
            latch.countDown();
        }

        @Override
        public void onUploadProgress(long bytesWritten, long contentLength, boolean done) {
            uploadProgress.add(new Progress(bytesWritten, contentLength, done));
        }

        @Override
        public void onDownloadProgress(long bytesRead, long contentLength, boolean done) {
            downloadProgress.add(new Progress(bytesRead, contentLength, done));
        }

        public boolean isDone() {
            return done;
        }

        public boolean isSuccess() {
            return success;
        }

        public ApiException getException() {
            return exception;
        }

        public T getResult() {
            return result;
        }

        public List<Progress> getUploadProgress() {
            return new ArrayList<Progress>(uploadProgress);
        }

        public List<Progress> getDownloadProgress() {
            return new ArrayList<Progress>(downloadProgress);
        }
    }

    private static class Progress {
        public final long bytes;
        public final long contentLength;
        public final boolean done;

        public Progress(long bytes, long contentLength, boolean done) {
            this.bytes = bytes;
            this.contentLength = contentLength;
            this.done = done;
        }

        @Override
        public String toString() {
            return "<Progress " + bytes + " " + contentLength + " " + done + ">";
        }
    }
}
