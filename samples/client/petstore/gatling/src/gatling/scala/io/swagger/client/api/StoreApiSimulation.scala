package io.swagger.client.api

import io.swagger.client.model._
import com.typesafe.config.ConfigFactory

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.structure.PopulationBuilder

import java.io.File

import scala.collection.mutable

class StoreApiSimulation extends Simulation {

    def getCurrentDirectory = new File("").getAbsolutePath
    def userDataDirectory = getCurrentDirectory + "/src/gatling/resources/data"

    val config = ConfigFactory.load("default.conf")
    val durationSeconds = config.getInt("performance.durationSeconds")
    val warmUpSeconds = config.getInt("performance.warmUpSeconds")
    val authentication = config.getString("performance.authorizationHeader")
    val acceptHeader = config.getString("performance.acceptType")
    val contentTypeHeader = config.getString("performance.contentType")

    // Setup http protocol configuration
    val httpConf = http
        .baseURL("http://petstore.swagger.io/v2")
        .doNotTrackHeader("1")
        .acceptLanguageHeader("en-US,en;q=0.5")
        .acceptEncodingHeader("gzip, deflate")
        .userAgentHeader("Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0")
        .acceptHeader(acceptHeader)
        .contentTypeHeader(contentTypeHeader)

    // set authorization header if it has been modified from config
    if(!authentication.equals("~MANUAL_ENTRY")){
        httpConf.authorizationHeader(authentication)
    }

    // Setup all the operations per second for the test to ultimately be generated from configs
    val deleteOrderPerSecond = config.getDouble("performance.operationsPerSecond.deleteOrder")
    val getInventoryPerSecond = config.getDouble("performance.operationsPerSecond.getInventory")
    val getOrderByIdPerSecond = config.getDouble("performance.operationsPerSecond.getOrderById")
    val placeOrderPerSecond = config.getDouble("performance.operationsPerSecond.placeOrder")

    val scenarioBuilders: mutable.MutableList[PopulationBuilder] = new mutable.MutableList[PopulationBuilder]()

    // Set up CSV feeders
    val deleteOrderPATHFeeder = csv(userDataDirectory + File.separator + "deleteOrder-pathParams.csv").random
    val getOrderByIdPATHFeeder = csv(userDataDirectory + File.separator + "getOrderById-pathParams.csv").random
    val placeOrderBodyFeeder = csv(userDataDirectory + File.separator + "placeOrder-bodyParams.csv", escapeChar = '\\').random

    // Setup all scenarios

    
    val scndeleteOrder = scenario("deleteOrderSimulation")
        .exec(http("deleteOrder")
        .httpRequest("DELETE","/store/order/${orderId}")
        )

    // Run scndeleteOrder with warm up and reach a constant rate for entire duration
    scenarioBuilders += scndeleteOrder.inject(rampUsersPerSec(1) to(deleteOrderPerSecond) during(warmUpSeconds), constantUsersPerSec(deleteOrderPerSecond) during(durationSeconds))

    
    val scngetInventory = scenario("getInventorySimulation")
        .exec(http("getInventory")
        .httpRequest("GET","/store/inventory")
        )

    // Run scngetInventory with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetInventory.inject(rampUsersPerSec(1) to(getInventoryPerSecond) during(warmUpSeconds), constantUsersPerSec(getInventoryPerSecond) during(durationSeconds))

    
    val scngetOrderById = scenario("getOrderByIdSimulation")
        .exec(http("getOrderById")
        .httpRequest("GET","/store/order/${orderId}")
        )

    // Run scngetOrderById with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetOrderById.inject(rampUsersPerSec(1) to(getOrderByIdPerSecond) during(warmUpSeconds), constantUsersPerSec(getOrderByIdPerSecond) during(durationSeconds))

    
    val scnplaceOrder = scenario("placeOrderSimulation")
        .feed(placeOrderBodyFeeder)
        .exec(http("placeOrder")
        .httpRequest("POST","/store/order")
        .body(StringBody(Order.toStringBody("${id}","${shipDate}","${complete}","${quantity}","${status}","${petId}")))
        )

    // Run scnplaceOrder with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnplaceOrder.inject(rampUsersPerSec(1) to(placeOrderPerSecond) during(warmUpSeconds), constantUsersPerSec(placeOrderPerSecond) during(durationSeconds))



    setUp(
        scenarioBuilders.toList
    ).protocols(httpConf)

}
