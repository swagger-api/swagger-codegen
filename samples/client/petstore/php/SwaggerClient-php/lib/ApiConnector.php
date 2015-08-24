<?php
/**
 *  Copyright 2015 SmartBear Software
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace Swagger\Client;

interface APIConnector {
  /**
   * set the HTTP timeout value
   *
   * @param integer $micro_seconds Number of micro seconds before timing out [set to 0 for no timeout]
   */
  public function setTimeout($micro_seconds);

  /**
   * get the HTTP timeout value
   *
   * @return string HTTP timeout value
   */
  public function getTimeout();

  /**
   * Returns the API host.
   *
   * @return string
   */
  public function getHost();

  /**
   * Sets data serializer.
   *
   * @param ObjectSerializer $serializer data serializer/sanitizer
   */
  public function setSerializer(ObjectSerializer $serializer);

  /**
   * Returns the data serializer.
   *
   * @return ObjectSerializer
   */
  public function getSerializer();

  /**
   * get the user agent of the api client
   *
   * @return string user agent
   */
  public function getUserAgent();

  /**
   * set the configuration
   *
   *  @param Configuration $config new configuration
   */
  public function setConfig($config);

  /**
   * get the current configuration
   *
   * @return Configuration
   */
  public function getConfig();

  /**
   * Sends API request to a given resource.
   *
   * @param string $uri path to a resource (i.e. '/pet/2')
   * @param string $method request method ($this->GET, $this->POST, etc.)
   * @param array $queryParams list of parameters (optional)
   * @param string $postData request payload (optional)
   * @param string $acceptTypes preferable response content types
   * @param string $contentTypes content type of request being sent
   * @param array $headers additional headers
   * @param string $responseType expected response type of the endpoint
   * @return string decoded response
   * @throws ApiException
   */
  public function send($uri, $method, $queryParams, $postData, $acceptTypes, $contentTypes,
                       $headers, $responseType);
}
