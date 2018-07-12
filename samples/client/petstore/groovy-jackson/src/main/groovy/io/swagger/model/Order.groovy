package io.swagger.model

import groovy.transform.Canonical
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty

@Canonical
@JsonInclude(JsonInclude.Include.NON_NULL)

class Order  {
    
    @JsonProperty('id')
    Long id = null
    
    @JsonProperty('petId')
    Long petId = null
    
    @JsonProperty('quantity')
    Integer quantity = null
    
    @JsonProperty('shipDate')
    OffsetDateTime shipDate = null
        /**
    * Order Status
    */
    enum StatusEnum {
        PLACED("placed"),
        APPROVED("approved"),
        DELIVERED("delivered")
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

    /* Order Status */

    @JsonProperty('status')
    StatusEnum status = null
    
    @JsonProperty('complete')
    Boolean complete = false

}


