package io.swagger.model

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty

@Canonical
@JsonInclude(JsonInclude.Include.NON_NULL)

class User  {
    
    @JsonProperty('id')
    Long id = null
    
    @JsonProperty('username')
    String username = null
    
    @JsonProperty('firstName')
    String firstName = null
    
    @JsonProperty('lastName')
    String lastName = null
    
    @JsonProperty('email')
    String email = null
    
    @JsonProperty('password')
    String password = null
    
    @JsonProperty('phone')
    String phone = null
    /* User Status */

    @JsonProperty('userStatus')
    Integer userStatus = null

}


