package io.swagger.client.apis

import io.swagger.client.infrastructure.ApiClient
import io.swagger.client.models.Order
import org.junit.Assert
import org.junit.Before
import org.junit.Test

/**
 * API tests for StoreApi
 */
class StoreApiTest {

    private lateinit var api: StoreApi

    @Before
    fun setup() {
        api = ApiClient().createService(StoreApi::class.java)
    }

    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     */
    @Test
    fun deleteOrderTest() {
        val orderId: kotlin.String? = null
        // val response = api.deleteOrder(orderId).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     */
    @Test
    fun getInventoryTest() {
        // val response = api.getInventory().execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     */
    @Test
    fun getOrderByIdTest() {
        val orderId: kotlin.Long? = null
        // val response = api.getOrderById(orderId).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Place an order for a pet
     *
     * 
     */
    @Test
    fun placeOrderTest() {
        val body: Order? = null
        // val response = api.placeOrder(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }
}
