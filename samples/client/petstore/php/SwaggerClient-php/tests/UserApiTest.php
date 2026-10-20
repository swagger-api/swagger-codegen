<?php

namespace Swagger\Client;

use Swagger\Client\Api\UserApi;

class UserApiTest extends \PHPUnit\Framework\TestCase
{

    /** @var UserApi*/
    private $api;

    public function setUp(): void
    {
        $this->api = new Api\UserApi();
    }

    // test login use
    public function testLoginUser()
    {
        // initialize the API client
        // login
        $response = $this->api->loginUser('xxxxx', 'yyyyyyyy');
        
        $this->assertIsString($response);
        $this->assertRegExp(
            '/logged in user session/',
            $response,
            "response string starts with 'logged in user session'"
        );
    }
}
