<?php

namespace Swagger\Client;

use GuzzleHttp\Psr7\Response;
use Http\Client\HttpClient;
use Psr\Http\Message\RequestInterface;
use Psr\Http\Message\ResponseInterface;

class FakeHttpClient implements HttpClient
{
    /** @var  RequestInterface|null */
    private $request;

    /**
     * Sends a PSR-7 request.
     *
     * @param RequestInterface $request
     *
     * @return ResponseInterface
     *
     * @throws \Http\Client\Exception If an error happens during processing the request.
     * @throws \Exception             If processing the request is impossible (eg. bad configuration).
     */
    public function sendRequest(RequestInterface $request)
    {
        $this->request = $request;
        return new Response(200);
    }

    /**
     * @return null|RequestInterface
     */
    public function getLastRequest()
    {
        return $this->request;
    }
}