/**
 * Swagger Petstore
 * This is a sample Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/). 
 *
 * OpenAPI spec version: 1.0.0
 * Contact: apiteam@swagger.io
 *
 * NOTE: This class is auto generated by the swagger code generator program.
 * https://github.com/swagger-api/swagger-codegen.git
 * Do not edit the class manually.
 */
package io.swagger.client.models

import io.swagger.client.models.Category

/**
 * 
 * @param category 
 * @param category2 
 * @param pets 
 */
data class SubCategory (

    val category: AllOfSubCategoryCategory? = null,
    val category2: Category? = null,
    val pets: kotlin.Array<AllOfSubCategoryPetsItems>? = null
) {
}