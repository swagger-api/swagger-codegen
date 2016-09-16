package io.swagger.handler;

import com.networknt.config.Config;
import com.networknt.server.HandlerProvider;
import io.undertow.Handlers;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Methods;

public class PathHandlerProvider implements HandlerProvider {

    public HttpHandler getHandler() {
        HttpHandler handler = Handlers.routing()


            .add(Methods.POST, "/pet", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("addPet");
                        }
                    })


            .add(Methods.DELETE, "/pet/{petId}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("deletePet");
                        }
                    })


            .add(Methods.GET, "/pet/findByStatus", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("findPetsByStatus");
                        }
                    })


            .add(Methods.GET, "/pet/findByTags", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("findPetsByTags");
                        }
                    })


            .add(Methods.GET, "/pet/{petId}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("getPetById");
                        }
                    })


            .add(Methods.PUT, "/pet", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("updatePet");
                        }
                    })


            .add(Methods.POST, "/pet/{petId}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("updatePetWithForm");
                        }
                    })


            .add(Methods.POST, "/pet/{petId}/uploadImage", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("uploadFile");
                        }
                    })


            .add(Methods.DELETE, "/store/order/{orderId}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("deleteOrder");
                        }
                    })


            .add(Methods.GET, "/store/inventory", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("getInventory");
                        }
                    })


            .add(Methods.GET, "/store/order/{orderId}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("getOrderById");
                        }
                    })


            .add(Methods.POST, "/store/order", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("placeOrder");
                        }
                    })


            .add(Methods.POST, "/user", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("createUser");
                        }
                    })


            .add(Methods.POST, "/user/createWithArray", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("createUsersWithArrayInput");
                        }
                    })


            .add(Methods.POST, "/user/createWithList", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("createUsersWithListInput");
                        }
                    })


            .add(Methods.DELETE, "/user/{username}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("deleteUser");
                        }
                    })


            .add(Methods.GET, "/user/{username}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("getUserByName");
                        }
                    })


            .add(Methods.GET, "/user/login", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("loginUser");
                        }
                    })


            .add(Methods.GET, "/user/logout", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("logoutUser");
                        }
                    })


            .add(Methods.PUT, "/user/{username}", new HttpHandler() {
                        public void handleRequest(HttpServerExchange exchange) throws Exception {
                            exchange.getResponseSender().send("updateUser");
                        }
                    })

        ;
        return handler;
    }
}

