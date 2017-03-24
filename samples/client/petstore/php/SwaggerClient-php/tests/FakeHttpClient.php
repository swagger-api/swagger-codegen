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
    /** @var  ResponseInterface|null */
    private $response;

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
        return $this->response ?: new Response(200);
    }

    /**
     * @return null|RequestInterface
     */
    public function getLastRequest()
    {
        return $this->request;
    }

    /**
     * @param null|ResponseInterface $response
     */
    public function setResponse(ResponseInterface $response = null)
    {
        $this->response = $response;
    }
}