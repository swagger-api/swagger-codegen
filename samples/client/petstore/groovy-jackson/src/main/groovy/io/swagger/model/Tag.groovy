package io.swagger.model

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty

@Canonical
@JsonInclude(JsonInclude.Include.NON_NULL)

class Tag  {
    
    @JsonProperty('id')
    Long id = null
    
    @JsonProperty('name')
    String name = null

}


