package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Results;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;


import swagger.SwaggerUtils.ApiAction;


public class StoreApiController extends Controller {

    private final StoreApiControllerImpInterface imp;
    private final ObjectMapper mapper;

    @Inject
    private StoreApiController(StoreApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result deleteOrder(String orderId) throws Exception {
        imp.deleteOrder(orderId);
        return Results.status(200);
    }

    @ApiAction
    public Result getInventory() throws Exception {
        Map<String, Integer> obj = imp.getInventory();
        JsonNode result = mapper.valueToTree(obj);
        return Results.status(200, result);
    }

    @ApiAction
    public Result getOrderById(Long orderId) throws Exception {
        Order obj = imp.getOrderById(orderId);
        JsonNode result = mapper.valueToTree(obj);
        return Results.status(200, result);
    }

    @ApiAction
    public Result placeOrder() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        Order obj = imp.placeOrder(body);
        JsonNode result = mapper.valueToTree(obj);
        return Results.status(200, result);
    }
}
