package io.swagger.api.impl;

import java.util.Map;
import io.swagger.model.Order;

import io.swagger.api.StoreApi;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;

import java.util.List;

@Path("/store")

@Api(description = "the store API")




public class StoreApiServiceImpl implements StoreApi {

    public Response deleteOrder(String orderId) {
        return Response.ok().entity("magic!").build();
    }

    public Response getInventory() {
        return Response.ok().entity("magic!").build();
    }

    public Response getOrderById(Long orderId) {
        return Response.ok().entity("magic!").build();
    }

    public Response placeOrder(Order body) {
        return Response.ok().entity("magic!").build();
    }
}

