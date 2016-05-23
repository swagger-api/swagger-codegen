package io.swagger.client.api;

import java.util.Map;

import io.swagger.client.model.Order;
import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.Path;

public interface StoreApi {
    /**
     * Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything
     * above 1000 or nonintegers will generate API errors
     *
     * @param orderId ID of the order that needs to be deleted (required)
     * @return Call<Void>
     */

    @DELETE("store/order/{orderId}")
    Call<Void> deleteOrder(
            @Path("orderId") String orderId
    );

    /**
     * Returns pet inventories by status Returns a map of status codes to quantities
     *
     * @return Call<Map<String, Integer>>
     */

    @GET("store/inventory")
    Call<Map<String, Integer>> getInventory();


    /**
     * Find purchase order by ID For valid response try integer IDs with value &lt;&#x3D; 5 or &gt;
     * 10. Other values will generated exceptions
     *
     * @param orderId ID of pet that needs to be fetched (required)
     * @return Call<Order>
     */

    @GET("store/order/{orderId}")
    Call<Order> getOrderById(
            @Path("orderId") Long orderId
    );

    /**
     * Place an order for a pet
     *
     * @param body order placed for purchasing the pet (required)
     * @return Call<Order>
     */

    @POST("store/order")
    Call<Order> placeOrder(
            @Body Order body
    );

}
