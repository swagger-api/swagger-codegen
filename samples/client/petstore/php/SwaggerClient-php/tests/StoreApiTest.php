<?php

namespace Swagger\Client;

use Swagger\Client\Api\StoreApi;

class StoreApiTest extends \PHPUnit_Framework_TestCase
{
    /** @var  StoreApi */
    private $api;

    public function setUp()
    {
        $this->api = new Api\StoreApi();
    }

    public function testGetInventory()
    {
        $result = $this->api->getInventory();

        $this->assertInternalType("array", $result);
        $this->assertInternalType("int", $result['available']);
    }

    /*
     * comment out as we've removed invalid endpoints from the spec, we'll introduce something
     * similar in the future when we've time to update the petstore server
     *
    // test get inventory
    public function testGetInventoryInObject()
    {
        $result = $this->api->getInventoryInObject();

        $this->assertInternalType("array", $result);
        $this->assertInternalType("int", $result['available']);
    }
     */
}
