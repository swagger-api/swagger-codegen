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
            .add(Methods.GET, "/v2/fake", new FakeGetHandler())
            .add(Methods.PATCH, "/v2/fake", new FakePatchHandler())
            .add(Methods.POST, "/v2/fake", new FakePostHandler())
            .add(Methods.GET, "/v2/pet/findByStatus", new PetFindByStatusGetHandler())
            .add(Methods.GET, "/v2/pet/findByTags", new PetFindByTagsGetHandler())
            .add(Methods.DELETE, "/v2/pet/{petId}", new PetPetIdDeleteHandler())
            .add(Methods.GET, "/v2/pet/{petId}", new PetPetIdGetHandler())
            .add(Methods.POST, "/v2/pet/{petId}", new PetPetIdPostHandler())
            .add(Methods.POST, "/v2/pet/{petId}/uploadImage", new PetPetIdUploadImagePostHandler())
            .add(Methods.POST, "/v2/pet", new PetPostHandler())
            .add(Methods.PUT, "/v2/pet", new PetPutHandler())
            .add(Methods.GET, "/v2/store/inventory", new StoreInventoryGetHandler())
            .add(Methods.DELETE, "/v2/store/order/{orderId}", new StoreOrderOrderIdDeleteHandler())
            .add(Methods.GET, "/v2/store/order/{orderId}", new StoreOrderOrderIdGetHandler())
            .add(Methods.POST, "/v2/store/order", new StoreOrderPostHandler())
            .add(Methods.POST, "/v2/user/createWithArray", new UserCreateWithArrayPostHandler())
            .add(Methods.POST, "/v2/user/createWithList", new UserCreateWithListPostHandler())
            .add(Methods.GET, "/v2/user/login", new UserLoginGetHandler())
            .add(Methods.GET, "/v2/user/logout", new UserLogoutGetHandler())
            .add(Methods.POST, "/v2/user", new UserPostHandler())
            .add(Methods.DELETE, "/v2/user/{username}", new UserUsernameDeleteHandler())
            .add(Methods.GET, "/v2/user/{username}", new UserUsernameGetHandler())
            .add(Methods.PUT, "/v2/user/{username}", new UserUsernamePutHandler())
        ;
        return handler;
    }
}

