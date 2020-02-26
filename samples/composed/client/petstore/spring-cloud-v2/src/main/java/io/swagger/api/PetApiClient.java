package io.swagger.api;

import org.springframework.cloud.openfeign.FeignClient;
import io.swagger.configuration.ClientConfiguration;

@FeignClient(contextId="PetApiClient", name="${swaggerPetstore.name:swaggerPetstore}", url="${swaggerPetstore.url:/}", configuration = ClientConfiguration.class)
public interface PetApiClient extends PetApi {
}
