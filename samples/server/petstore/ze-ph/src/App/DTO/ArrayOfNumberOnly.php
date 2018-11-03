<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class ArrayOfNumberOnly
{
    /**
     * @DTA\Data(field="ArrayNumber", nullable=true)
     * @var float[]
     */
    public $array_number;
}
