<?php

namespace Swagger\Client;

use Swagger\Client\Api\PetApi;
use Swagger\Client\Model\Category;
use Swagger\Client\Model\Pet;
use Swagger\Client\Model\Tag;

require_once __DIR__ . '/HandwrittenPet.php';

class ReturnTypeTest extends \PHPUnit_Framework_TestCase
{
    /** @var PetApi */
    private $api;

    /** @var  int */
    private $petId;

    public function setUp()
    {
        parent::setUp();
        $this->api = new PetApi();

        $this->petId = 10005;
        $pet = new Pet;
        $pet->setId($this->petId);
        $pet->setName("PHP Unit Test");
        $pet->setPhotoUrls(array("http://test_php_unit_test.com"));

        // add a new pet (model)
        list(, $status) = $this->api->addPetWithHttpInfo($pet);
        $this->assertSame(200, $status);
    }

    public function testSetReturnType()
    {
        $this->api->setReturnType('\Swagger\Client\HandwrittenPet');
        $this->assertInstanceOf(HandwrittenPet::class, $this->api->getPetById($this->petId));

        list($pet) = $this->api->findPetsByStatus(Pet::STATUS_SOLD);
        $this->assertInstanceOf(HandwrittenPet::class, $pet);
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testSetReturnTypeThrowsException()
    {
        $this->api->setReturnType('\Swagger\Client\UnknownReturnType');
    }
}
