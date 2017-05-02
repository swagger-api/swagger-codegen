<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Definition 2
 */
class Definition1 
{
    /**
     * @DTA\Data(field="Definition2", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\Definition2::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\Definition2::class})
     * @var \App\DTO\Definition2
     */
    public $definition2;
    /**
     * Content Id for lookup
     * @DTA\Data(field="Id", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $id;
}

