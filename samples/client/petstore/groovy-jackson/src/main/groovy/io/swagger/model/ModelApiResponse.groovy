package io.swagger.model

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty

@Canonical
@JsonInclude(JsonInclude.Include.NON_NULL)

class ModelApiResponse  {
    
    @JsonProperty('code')
    Integer code = null
    
    @JsonProperty('type')
    String type = null
    
    @JsonProperty('message')
    String message = null

}


