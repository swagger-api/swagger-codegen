<?php

namespace Swagger\Client;

use Swagger\Client\Api\FakeApi;
use Swagger\Client\Api\UserApi;

require_once __DIR__ . '/FakeHttpClient.php';

class HeadersTest extends \PHPUnit_Framework_TestCase
{
    /** @var  FakeHttpClient */
    private $fakeHttpClient;

    public function setUp()
    {
        $this->fakeHttpClient = new FakeHttpClient();
    }

    public function testDefaultHeaders()
    {
        $config = new Configuration();
        $config->addDefaultHeader('someHeader', 'someValue');
        $api = new Api\PetApi($this->fakeHttpClient, $config);

        $api->getPetById(3);

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('someHeader', $headers);
        $this->assertEquals(['someValue'], $headers['someHeader']);
    }

    public function testDefaultHeadersMayBeOverwritten()
    {
        $config = new Configuration();
        $config->addDefaultHeader('Accept', 'text/plain');
        $config->addDefaultHeader('Content-Type', 'text/plain');
        $config->addDefaultHeader('Something-Else', 'text/plain');
        $api = new Api\PetApi($this->fakeHttpClient, $config);

        $api->getPetById(3);

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('Accept', $headers);
        $this->assertEquals(['application/json'], $headers['Accept']);

        $this->assertArrayHasKey('Content-Type', $headers);
        $this->assertEquals(['application/json'], $headers['Content-Type']);

        $this->assertArrayHasKey('Something-Else', $headers);
        $this->assertEquals(['text/plain'], $headers['Something-Else']);
    }
}
