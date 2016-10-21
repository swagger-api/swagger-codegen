package io.swagger.handler;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;

public class PetFindByTagsGetHandler implements HttpHandler {

    public void handleRequest(HttpServerExchange exchange) throws Exception {
        exchange.getResponseSender().send("findPetsByTags");
    }
}
