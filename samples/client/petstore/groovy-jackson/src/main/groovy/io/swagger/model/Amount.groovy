package io.swagger.model

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty

@Canonical
@JsonInclude(JsonInclude.Include.NON_NULL)

class Amount  {
    /* some description  */

    @JsonProperty('value')
    Double value = null
    
    @JsonProperty('currency')
    String currency = null

}


