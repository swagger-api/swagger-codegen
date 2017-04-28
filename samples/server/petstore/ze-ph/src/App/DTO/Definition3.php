<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Definition 3 (recursive)
 */
class Definition3 
{
    /**
     * nested Definition3 array
     * @DTA\Data(field="Definition3", nullable=true)
     * @DTA\Strategy(name="ObjectArray", options={"type":\App\DTO\Definition3::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":\App\DTO\Definition3::class}}
     * }})
     * @var \App\DTO\Definition3[]
     */
    public $definition3;
    /**
     * Content Id for lookup
     * @DTA\Data(field="Id", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $id;
}

