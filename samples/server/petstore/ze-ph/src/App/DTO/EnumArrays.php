<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class EnumArrays 
{
    /**
     * @DTA\Data(field="just_symbol", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $just_symbol;
    /**
     * @DTA\Data(field="array_enum", nullable=true)
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var string[]
     */
    public $array_enum;
    /**
     * @DTA\Data(field="array_array_enum", nullable=true)
     * @DTA\Strategy(name="ObjectArray", options={"type":string[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":string[]::class}}
     * }})
     * @var string[][]
     */
    public $array_array_enum;
}

