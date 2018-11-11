package io.swagger.model

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty

@Canonical
@JsonInclude(JsonInclude.Include.NON_NULL)

class Pet  {
    
    @JsonProperty('id')
    Long id = null
    
    @JsonProperty('category')
    Category category = null
    
    @JsonProperty('name')
    String name = null
    
    @JsonProperty('photoUrls')
    List<String> photoUrls = new ArrayList<>()
    
    @JsonProperty('tags')
    List<Tag> tags = new ArrayList<>()
        /**
    * pet status in the store
    */
    enum StatusEnum {
        AVAILABLE("available"),
        PENDING("pending"),
        SOLD("sold")
        private String value;

        StatusEnum(String value) {
            this.value = value
        }


        @JsonValue
        public String getValue() {
            return value;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }


        @JsonCreator
            public static StatusEnum fromValue(String text) {
        return StatusEnum.values().findFirst { b ->
            return String.valueOf(b.value).equals(text))
        }
    }
    }

    /* pet status in the store */

    @JsonProperty('status')
    StatusEnum status = null

}


