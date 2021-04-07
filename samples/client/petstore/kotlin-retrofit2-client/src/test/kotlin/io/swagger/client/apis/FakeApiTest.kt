package io.swagger.client.apis

import io.swagger.client.infrastructure.ApiClient
import io.swagger.client.models.Client
import io.swagger.client.models.OuterBoolean
import io.swagger.client.models.OuterComposite
import io.swagger.client.models.OuterNumber
import io.swagger.client.models.OuterString
import io.swagger.client.models.User
import org.junit.Assert
import org.junit.Before
import org.junit.Test

/**
 * API tests for FakeApi
 */
class FakeApiTest {

    private lateinit var api: FakeApi

    @Before
    fun setup() {
        api = ApiClient().createService(FakeApi::class.java)
    }

    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    @Test
    fun fakeOuterBooleanSerializeTest() {
        val body: OuterBoolean? = null
        // val response = api.fakeOuterBooleanSerialize(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * 
     *
     * Test serialization of object with outer number type
     */
    @Test
    fun fakeOuterCompositeSerializeTest() {
        val body: OuterComposite? = null
        // val response = api.fakeOuterCompositeSerialize(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * 
     *
     * Test serialization of outer number types
     */
    @Test
    fun fakeOuterNumberSerializeTest() {
        val body: OuterNumber? = null
        // val response = api.fakeOuterNumberSerialize(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * 
     *
     * Test serialization of outer string types
     */
    @Test
    fun fakeOuterStringSerializeTest() {
        val body: OuterString? = null
        // val response = api.fakeOuterStringSerialize(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * 
     *
     * 
     */
    @Test
    fun testBodyWithQueryParamsTest() {
        val body: User? = null
        val query: kotlin.String? = null
        // val response = api.testBodyWithQueryParams(body, query).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Test
    fun testClientModelTest() {
        val body: Client? = null
        // val response = api.testClientModel(body).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     */
    @Test
    fun testEndpointParametersTest() {
        val number: java.math.BigDecimal? = null
        val double: kotlin.Double? = null
        val patternWithoutDelimiter: kotlin.String? = null
        val byte: kotlin.ByteArray? = null
        val integer: kotlin.Int? = null
        val int32: kotlin.Int? = null
        val int64: kotlin.Long? = null
        val float: kotlin.Float? = null
        val string: kotlin.String? = null
        val binary: kotlin.Array<kotlin.Byte>? = null
        val date: java.time.OffsetDateTime? = null
        val dateTime: java.time.OffsetDateTime? = null
        val password: kotlin.String? = null
        val callback: kotlin.String? = null
        // val response = api.testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    @Test
    fun testEnumParametersTest() {
        val enumFormStringArray: kotlin.collections.List<kotlin.String>? = null
        val enumFormString: kotlin.String? = null
        val enumHeaderStringArray: kotlin.collections.List<kotlin.String>? = null
        val enumHeaderString: kotlin.String? = null
        val enumQueryStringArray: kotlin.collections.List<kotlin.String>? = null
        val enumQueryString: kotlin.String? = null
        val enumQueryInteger: kotlin.Int? = null
        val enumQueryDouble: kotlin.Double? = null
        // val response = api.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * test inline additionalProperties
     *
     * 
     */
    @Test
    fun testInlineAdditionalPropertiesTest() {
        val param: kotlin.Any? = null
        // val response = api.testInlineAdditionalProperties(param).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }

    /**
     * test json serialization of form data
     *
     * 
     */
    @Test
    fun testJsonFormDataTest() {
        val param: kotlin.String? = null
        val param2: kotlin.String? = null
        // val response = api.testJsonFormData(param, param2).execute()
        // Assert.assertEquals(200, response.code())
        // TODO: test validations
    }
}
