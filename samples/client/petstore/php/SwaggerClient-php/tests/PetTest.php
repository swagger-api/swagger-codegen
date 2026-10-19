<?php

namespace Swagger\Client;

use Swagger\Client\Model\Pet;

class PetTest extends \PHPUnit\Framework\TestCase
{
    /**
     * test empty object serialization
     */
    public function testEmptyPetSerialization()
    {
        $new_pet = new Pet;
        // the empty object should be serialised to {}
        $this->assertSame("{}", "$new_pet");
    }
}
