package io.swagger.api;

import org.springframework.cloud.openfeign.FeignClient;
import io.swagger.configuration.ClientConfiguration;

@FeignClient(contextId="StoreApiClient", name="${swaggerPetstore.name:swaggerPetstore}", url="${swaggerPetstore.url:/}", configuration = ClientConfiguration.class)
public interface StoreApiClient extends StoreApi {
}
