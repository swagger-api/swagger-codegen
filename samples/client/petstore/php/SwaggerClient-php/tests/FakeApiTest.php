<?php

namespace Swagger\Client;

use Swagger\Client\Api\FakeApi;

require_once __DIR__ . '/FakeHttpClient.php';

class FakeApiTest extends \PHPUnit_Framework_TestCase
{

    /** @var  FakeHttpClient */
    private $fakeHttpClient;
    /** @var  FakeApi */
    private $api;

    public function setUp()
    {
        $this->fakeHttpClient = new FakeHttpClient();
        $this->api = new Api\FakeApi($this->fakeHttpClient);
    }

    public function testHeaderParam()
    {
        $this->api->testEnumParameters([], [], [], 'something');

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('enum_header_string', $headers);
        $this->assertEquals(['something'], $headers['enum_header_string']);
    }

    public function testHeaderParamCollection()
    {
        $this->api->testEnumParameters([], [], ['string1', 'string2']);

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('enum_header_string_array', $headers);
        $this->assertEquals(['string1,string2'], $headers['enum_header_string_array']);
    }
}
