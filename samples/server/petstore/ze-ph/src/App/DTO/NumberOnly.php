<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class NumberOnly
{
    /**
     * @DTA\Data(field="JustNumber", nullable=true)
     * @var float
     */
    public $just_number;
}
