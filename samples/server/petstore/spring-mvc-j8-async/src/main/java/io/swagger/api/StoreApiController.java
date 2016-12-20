package io.swagger.api;

import org.springframework.stereotype.Controller;



@Controller
public class StoreApiController implements StoreApi {



    public Callable<ResponseEntity<Void>> deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathVariable("orderId") String orderId) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Map<String, Integer>>> getInventory() {
        // do some magic!
        return new Callable<ResponseEntity<Map<String, Integer>>>() {
            @Override
            public ResponseEntity<Map<String, Integer>> call() throws Exception {
                return new ResponseEntity<Map<String, Integer>>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Order>> getOrderById(@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("orderId") Long orderId) {
        // do some magic!
        return new Callable<ResponseEntity<Order>>() {
            @Override
            public ResponseEntity<Order> call() throws Exception {
                return new ResponseEntity<Order>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Order>> placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true ) @RequestBody Order body) {
        // do some magic!
        return new Callable<ResponseEntity<Order>>() {
            @Override
            public ResponseEntity<Order> call() throws Exception {
                return new ResponseEntity<Order>(HttpStatus.OK);
            }
        };
    }

}
