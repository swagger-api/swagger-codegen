package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

@RestController
public class StoreApiController implements StoreApi {

    private final StoreApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public StoreApiController(StoreApiDelegate delegate) {
        this.delegate = delegate;
    }
    public ResponseEntity<Void> deleteOrder(@Parameter(in = ParameterIn.PATH, description = "ID of the order that needs to be deleted", required=true, schema=@Schema()) @PathVariable("order_id") String orderId) {
        return delegate.deleteOrder(orderId);
    }

    public ResponseEntity<Map<String, Integer>> getInventory() {
        return delegate.getInventory();
    }

    public ResponseEntity<Order> getOrderById(@Min(1L) @Max(5L) @Parameter(in = ParameterIn.PATH, description = "ID of pet that needs to be fetched", required=true, schema=@Schema(allowableValues={  }, minimum="1", maximum="5"
)) @PathVariable("order_id") Long orderId) {
        return delegate.getOrderById(orderId);
    }

    public ResponseEntity<Order> placeOrder(@Parameter(in = ParameterIn.DEFAULT, description = "order placed for purchasing the pet", required=true, schema=@Schema()) @Valid @RequestBody Order body) {
        return delegate.placeOrder(body);
    }

}
