package io.swagger.api.impl;

import io.swagger.api.*;
import java.util.Map;
import io.swagger.model.Order;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;


/**
 * Swagger Petstore
 *
 * <p>This is a sample Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/). 
 *
 */
public class StoreApiServiceImpl implements StoreApi {
    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with positive integer value.\\ \\ Negative or non-integer values will generate API errors
     *
     */
    public void deleteOrder(Long orderId) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     *
     */
    public Map<String, Integer> getInventory() {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &gt;&#x3D; 1 and &lt;&#x3D; 10.\\ \\ Other values will generated exceptions
     *
     */
    public Order getOrderById(Long orderId) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Place an order for a pet
     *
     */
    public Order placeOrder(Order body) {
        // TODO: Implement...
        
        return null;
    }
    
}

