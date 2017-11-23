<?php

namespace Swagger\Client;

class ConfigurationTest extends \PHPUnit_Framework_TestCase
{
    /**
     * @test
     */
    public function getScheme()
    {
        $this->assertSame('http', (new Configuration())->getScheme());
    }

    /**
     * @test
     */
    public function setScheme()
    {
        $this->assertSame('ws', (new Configuration())->setScheme('ws')->getScheme());
    }

    /**
     * @test
     */
    public function getHost()
    {
        $this->assertSame('petstore.swagger.io:80', (new Configuration())->getHost());
    }

    /**
     * @test
     */
    public function setHost()
    {
        $this->assertSame('newpetstore.swagger.io:80', (new Configuration())->setHost('newpetstore.swagger.io:80')->getHost());
    }

    /**
     * @test
     */
    public function getBasePath()
    {
        $this->assertSame('/v2', (new Configuration())->getBasePath());
    }

    /**
     * @test
     */
    public function setBasePath()
    {
        $this->assertSame('/v3', (new Configuration())->setBasePath('/v3')->getBasePath());
    }

    /**
     * @test
     */
    public function getBaseUrl()
    {
        $configuration = new Configuration();
        $this->assertSame('http://petstore.swagger.io:80/v2', $configuration->getBaseUrl());

        $configuration->setHost('newpetstore.swagger.io:80/')
            ->setBasePath('/v3/');
        $this->assertSame('http://newpetstore.swagger.io:80/v3', $configuration->getBaseUrl());
    }
}
