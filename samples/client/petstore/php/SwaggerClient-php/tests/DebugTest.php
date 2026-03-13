<?php
namespace Swagger\Client;

class DebugTest extends \PHPUnit_Framework_TestCase
{
    const PET_ID = 10005;

    /** @var Api\PetApi */
    private $api;

    protected function setUp()
    {
        $newPetId = self::PET_ID;
        $newPet = new Model\Pet;
        $newPet->setId($newPetId);
        $newPet->setName("PHP Unit Test");
        $newPet->setPhotoUrls(["http://test_php_unit_test.com"]);

        $this->expectOutputRegex('#GET /v2/pet/' . self::PET_ID . ' HTTP/1.1#');

        $config = new Configuration();
        $config->setDebug(true);
        $this->api = new Api\PetApi(null, $config);
        $this->api->addPet($newPet);
    }


    public function testEnableDebugOutput()
    {
        $this->api->getPetById(self::PET_ID);
    }

    public function testEnableDebugOutputAsync()
    {
        $promise = $this->api->getPetByIdAsync(self::PET_ID);
        $promise->wait();
    }
}
