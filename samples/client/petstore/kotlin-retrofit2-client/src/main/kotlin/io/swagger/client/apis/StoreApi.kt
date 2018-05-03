package io.swagger.client.apis

import retrofit2.Call
import retrofit2.http.*

import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import io.swagger.client.models.Order

interface StoreApi {
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   * @return Call&lt;Void&gt;
   */
  @DELETE("store/order/{order_id}")
  fun deleteOrder(
    @retrofit2.http.Path("order_id") orderId: kotlin.String
  ): Call<Void>

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Call&lt;kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt;&gt;
   */
  @GET("store/inventory")
  fun getInventory()
    : Call<kotlin.collections.Map<kotlin.String, kotlin.Int>>

  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return Call&lt;Order&gt;
   */
  @GET("store/order/{order_id}")
  fun getOrderById(
    @retrofit2.http.Path("order_id") orderId: kotlin.Long
  ): Call<Order>

  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet (required)
   * @return Call&lt;Order&gt;
   */
  @POST("store/order")
  fun placeOrder(
    @retrofit2.http.Body body: Order
  ): Call<Order>

}
