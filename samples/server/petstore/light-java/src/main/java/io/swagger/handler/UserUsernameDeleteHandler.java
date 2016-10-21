package io.swagger.handler;

import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;

public class UserUsernameDeleteHandler implements HttpHandler {

    public void handleRequest(HttpServerExchange exchange) throws Exception {
        exchange.getResponseSender().send("deleteUser");
    }
}
