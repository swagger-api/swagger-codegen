<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Definition 2
 */
class Definition2 
{
    /**
     * nested Definition1 array
     * @DTA\Data(field="Definition1", nullable=true)
     * @DTA\Strategy(name="ObjectArray", options={"type":\App\DTO\Definition1::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":\App\DTO\Definition1::class}}
     * }})
     * @var \App\DTO\Definition1[]
     */
    public $definition1;
    /**
     * Content Id for lookup
     * @DTA\Data(field="Id", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $id;
}

