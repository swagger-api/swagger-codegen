<?php

namespace Swagger\Client;

// test object serializer
class ObjectSerializerTest extends \PHPUnit_Framework_TestCase
{
    /**
     * Test the sanitizeForSerialization method with a stdClass.
     */
    public function testSanitizeForSerializationWithStdClass()
    {
        // Initialize the ObjectSerializer.
        $s = new ObjectSerializer();
        
        // Build a stdClass object.
        $obj = new \stdClass();
        $obj->prop1 = 'val1';
        $obj->prop2 = 'val2';
        
        // Call the method.
        $serialized = $s->sanitizeForSerialization($obj);

        // Assert that the stdClass object is sanitized as expected.
        $this->assertEquals('val1', $serialized->prop1);
        $this->assertEquals('val2', $serialized->prop2);
    }
    
    // test sanitizeFilename
    public function testSanitizeFilename()
    {
        // initialize the API client
        $s = new ObjectSerializer();

        $this->assertSame("sun.gif", $s->sanitizeFilename("sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("../sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("/var/tmp/sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("./sun.gif"));
        
        $this->assertSame("sun", $s->sanitizeFilename("sun"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("..\sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("\var\tmp\sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("c:\var\tmp\sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename(".\sun.gif"));
    }
}
