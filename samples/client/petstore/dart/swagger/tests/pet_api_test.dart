import 'package:swagger/api.dart';
import 'package:test/test.dart';


/// tests for PetApi
void main() {
  var instance = new PetApi();

  group('tests for PetApi', () {
    // Add a new pet to the store
    //
    //Future addPet(Pet body) async
    test('test addPet', () async {
      // TODO
    });

    // Deletes a pet
    //
    //Future deletePet(int petId, { String apiKey }) async
    test('test deletePet', () async {
      // TODO
    });

    //Future<ApiResponse> doCategoryStuff({ SubCategory body }) async
    test('test doCategoryStuff', () async {
      // TODO
    });

    // Finds Pets by status
    //
    // Multiple status values can be provided with comma separated strings
    //
    //Future<List<Pet>> findPetsByStatus(List<String> status) async
    test('test findPetsByStatus', () async {
      // TODO
    });

    // Finds Pets by tags
    //
    // Muliple tags can be provided with comma separated strings. Use\\ \\ tag1, tag2, tag3 for testing.
    //
    //Future<List<Pet>> findPetsByTags(List<String> tags) async
    test('test findPetsByTags', () async {
      // TODO
    });

    //Future<AllPetsResponse> getAllPets() async
    test('test getAllPets', () async {
      // TODO
    });

    // Find pet by ID
    //
    // Returns a single pet
    //
    //Future<Pet> getPetById(int petId) async
    test('test getPetById', () async {
      // TODO
    });

    //Future<SinglePetResponse> getRandomPet() async
    test('test getRandomPet', () async {
      // TODO
    });

    // Update an existing pet
    //
    //Future updatePet(Pet body) async
    test('test updatePet', () async {
      // TODO
    });

    // Updates a pet in the store with form data
    //
    //Future updatePetWithForm(int petId, { String name, String status }) async
    test('test updatePetWithForm', () async {
      // TODO
    });

    // uploads an image
    //
    //Future<ApiResponse> uploadFile(int petId, { Object body }) async
    test('test uploadFile', () async {
      // TODO
    });

  });
}
