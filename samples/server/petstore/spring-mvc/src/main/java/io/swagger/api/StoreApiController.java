package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import io.swagger.annotations.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.beans.factory.annotation.Autowired;
import java.io.IOException;

import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.springframework.beans.factory.annotation.Qualifier;
import java.io.IOException;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class StoreApiController implements StoreApi {
    private final ObjectMapper objectMapper;
    private final XmlMapper xmlMapper;

    public StoreApiController(ObjectMapper objectMapper, XmlMapper xmlMapper) {
        this.objectMapper = objectMapper;
        this.xmlMapper = xmlMapper;
    }

    public ResponseEntity<Void> deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathVariable("order_id") String orderId,
        @RequestHeader("Accept") String accept) {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<Map<String, Integer>> getInventory(@RequestHeader("Accept") String accept) throws IOException {
        // do some magic!

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Map<String, Integer>>(getObjectMapperByContentType("application/json").readValue("{  \"key\" : 0}", Map.class), HttpStatus.OK);
        }

        return new ResponseEntity<Map<String, Integer>>(HttpStatus.OK);
    }

    public ResponseEntity<Order> getOrderById( @Min(1) @Max(5)@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("order_id") Long orderId,
        @RequestHeader("Accept") String accept) throws IOException {
        // do some magic!

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Order>(getObjectMapperByContentType("application/xml").readValue("<Order>  <id>123456789</id>  <petId>123456789</petId>  <quantity>123</quantity>  <shipDate>2000-01-23T04:56:07.000Z</shipDate>  <status>placed</status>  <complete>true</complete></Order>", Order.class), HttpStatus.OK);
        }


        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Order>(getObjectMapperByContentType("application/json").readValue("{  \"petId\" : 6,  \"quantity\" : 1,  \"id\" : 0,  \"shipDate\" : \"2000-01-23T04:56:07.000+00:00\",  \"complete\" : false,  \"status\" : \"placed\"}", Order.class), HttpStatus.OK);
        }

        return new ResponseEntity<Order>(HttpStatus.OK);
    }

    public ResponseEntity<Order> placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true )  @Valid @RequestBody Order body,
        @RequestHeader("Accept") String accept) throws IOException {
        // do some magic!

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Order>(getObjectMapperByContentType("application/xml").readValue("<Order>  <id>123456789</id>  <petId>123456789</petId>  <quantity>123</quantity>  <shipDate>2000-01-23T04:56:07.000Z</shipDate>  <status>placed</status>  <complete>true</complete></Order>", Order.class), HttpStatus.OK);
        }


        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Order>(getObjectMapperByContentType("application/json").readValue("{  \"petId\" : 6,  \"quantity\" : 1,  \"id\" : 0,  \"shipDate\" : \"2000-01-23T04:56:07.000+00:00\",  \"complete\" : false,  \"status\" : \"placed\"}", Order.class), HttpStatus.OK);
        }

        return new ResponseEntity<Order>(HttpStatus.OK);
    }

    /**
     * Returns the appropriate Object Mapper depending on the media type.
     * @param mediaType
     * @return
     */
    private ObjectMapper getObjectMapperByContentType(String mediaType){
        if(MediaType.APPLICATION_XML_VALUE.equals(mediaType)){
            return xmlMapper;
        } else {
            return objectMapper;
        }
    }
}
