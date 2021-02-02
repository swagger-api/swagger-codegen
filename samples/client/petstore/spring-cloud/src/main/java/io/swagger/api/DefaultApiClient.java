package io.swagger.api;

import org.springframework.cloud.openfeign.FeignClient;
import io.swagger.configuration.ClientConfiguration;

@FeignClient(contextId="DefaultApiClient", name="${swaggerPetstore.name:swaggerPetstore}", url="${swaggerPetstore.url:/}", configuration = ClientConfiguration.class)
public interface DefaultApiClient extends DefaultApi {
}
