<?php

require_once('SwaggerPetstore.php');

class PetApiTest extends \PHPUnit_Framework_TestCase
{
  // test getPetById and verify by the "id" of the response
  public function testGetPetById()
  {
    // initialize the API client
    $api_client = new SwaggerPetstore\APIClient('http://petstore.swagger.io/v2');
    $petId = 5;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerPetstore\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->getPetById($petId);
    $this->assertSame($response->id, $petId);
  }

 
  // test getPetByStatus and verify by the "id" of the response
  public function testFindPetByStatus()
  {
    // initialize the API client
    $api_client = new SwaggerPetstore\APIClient('http://petstore.swagger.io/v2');
    $pet_api = new SwaggerPetstore\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->findPetsByStatus("available");
    $this->assertGreaterThan(0, count($response)); // at least one object returned
    $this->assertSame(get_class($response[0]), "SwaggerPetstore\models\Pet"); // verify the object is Pet
    // loop through result to ensure status is "available" 
    foreach ($response as $_pet) {
      $this->assertSame($_pet['status'], "available");
    }

    // test invalid status 
    $response = $pet_api->findPetsByStatus("unknown_and_incorrect_status");
    $this->assertSame(count($response[0]), 0); // verify the object is Pet
  }

  // test updatePet and verify by the "id" of the response
  public function testUpdatePetWithForm()
  {
    // initialize the API client
    $api_client = new SwaggerPetstore\APIClient('http://petstore.swagger.io/v2');
    $petId = 99;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerPetstore\PetAPI($api_client);
    $update_response = $pet_api->updatePetWithForm($petId, 'programmer', 'coding');
    // return nothing (void)
    $this->assertSame($update_response, NULL);

    // verify updated Pet
    $response = $pet_api->getPetById($petId);

    $this->assertSame($response->id, $petId);
    $this->assertSame($response->status, 'coding');
    $this->assertSame($response->name, 'programmer');
  }

  // test updatePet and verify by the "id" of the response
  public function testAddPet()
  {
    // initialize the API client
    $api_client = new SwaggerPetstore\APIClient('http://petstore.swagger.io/v2');
    $new_pet_id = 10001;
    $new_pet = new SwaggerPetstore\models\Pet;
    $new_pet->id = $new_pet_id;
    $new_pet->name = "PHP Unit Test";

    $pet_api = new SwaggerPetstore\PetAPI($api_client);
    $add_response = $pet_api->addPet($new_pet);
    // return nothing (void)
    $this->assertSame($add_response, NULL);

    // verify updated Pet
    $response = $pet_api->getPetById(10001);

    $this->assertSame($response->id, $new_pet_id);
    $this->assertSame($response->name, 'PHP Unit Test');
  }



}

?>
