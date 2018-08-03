<?php

return PhpCsFixer\Config::create()
    ->setUsingCache(true)
    ->setRules([
        '@PSR2' => true,
        'ordered_imports' => true,
        'phpdoc_order' => true,
        'array_syntax' => [ 'syntax' => 'short' ],
        'strict_comparison' => true,
        'strict_param' => true
    ])
    ->setFinder(
        PhpCsFixer\Finder::create()
        ->in(__DIR__)
    );
