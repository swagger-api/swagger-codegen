package io.swagger.client.api

import io.swagger.client.model._
import com.typesafe.config.ConfigFactory

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.structure.PopulationBuilder

import java.io.File

import scala.collection.mutable

class UserApiSimulation extends Simulation {

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
    val createUserPerSecond = config.getDouble("performance.operationsPerSecond.createUser")
    val createUsersWithArrayInputPerSecond = config.getDouble("performance.operationsPerSecond.createUsersWithArrayInput")
    val createUsersWithListInputPerSecond = config.getDouble("performance.operationsPerSecond.createUsersWithListInput")
    val deleteUserPerSecond = config.getDouble("performance.operationsPerSecond.deleteUser")
    val getUserByNamePerSecond = config.getDouble("performance.operationsPerSecond.getUserByName")
    val loginUserPerSecond = config.getDouble("performance.operationsPerSecond.loginUser")
    val logoutUserPerSecond = config.getDouble("performance.operationsPerSecond.logoutUser")
    val updateUserPerSecond = config.getDouble("performance.operationsPerSecond.updateUser")

    val scenarioBuilders: mutable.MutableList[PopulationBuilder] = new mutable.MutableList[PopulationBuilder]()

    // Set up CSV feeders
    val createUserBodyFeeder = csv(userDataDirectory + File.separator + "createUser-bodyParams.csv", escapeChar = '\\').random
    val deleteUserPATHFeeder = csv(userDataDirectory + File.separator + "deleteUser-pathParams.csv").random
    val getUserByNamePATHFeeder = csv(userDataDirectory + File.separator + "getUserByName-pathParams.csv").random
    val loginUserQUERYFeeder = csv(userDataDirectory + File.separator + "loginUser-queryParams.csv").random
    val updateUserPATHFeeder = csv(userDataDirectory + File.separator + "updateUser-pathParams.csv").random
    val updateUserBodyFeeder = csv(userDataDirectory + File.separator + "updateUser-bodyParams.csv", escapeChar = '\\').random

    // Setup all scenarios

    
    val scncreateUser = scenario("createUserSimulation")
        .feed(createUserBodyFeeder)
        .exec(http("createUser")
        .httpRequest("POST","/user")
        .body(StringBody(User.toStringBody("${password}","${id}","${lastName}","${firstName}","${email}","${userStatus}","${phone}","${username}")))
        )

    // Run scncreateUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scncreateUser.inject(rampUsersPerSec(1) to(createUserPerSecond) during(warmUpSeconds), constantUsersPerSec(createUserPerSecond) during(durationSeconds))

    
    val scncreateUsersWithArrayInput = scenario("createUsersWithArrayInputSimulation")
        .exec(http("createUsersWithArrayInput")
        .httpRequest("POST","/user/createWithArray")
        .body(StringBody(StringBody("[]")))
        )

    // Run scncreateUsersWithArrayInput with warm up and reach a constant rate for entire duration
    scenarioBuilders += scncreateUsersWithArrayInput.inject(rampUsersPerSec(1) to(createUsersWithArrayInputPerSecond) during(warmUpSeconds), constantUsersPerSec(createUsersWithArrayInputPerSecond) during(durationSeconds))

    
    val scncreateUsersWithListInput = scenario("createUsersWithListInputSimulation")
        .exec(http("createUsersWithListInput")
        .httpRequest("POST","/user/createWithList")
        .body(StringBody(StringBody("[]")))
        )

    // Run scncreateUsersWithListInput with warm up and reach a constant rate for entire duration
    scenarioBuilders += scncreateUsersWithListInput.inject(rampUsersPerSec(1) to(createUsersWithListInputPerSecond) during(warmUpSeconds), constantUsersPerSec(createUsersWithListInputPerSecond) during(durationSeconds))

    
    val scndeleteUser = scenario("deleteUserSimulation")
        .exec(http("deleteUser")
        .httpRequest("DELETE","/user/${username}")
        )

    // Run scndeleteUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scndeleteUser.inject(rampUsersPerSec(1) to(deleteUserPerSecond) during(warmUpSeconds), constantUsersPerSec(deleteUserPerSecond) during(durationSeconds))

    
    val scngetUserByName = scenario("getUserByNameSimulation")
        .exec(http("getUserByName")
        .httpRequest("GET","/user/${username}")
        )

    // Run scngetUserByName with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetUserByName.inject(rampUsersPerSec(1) to(getUserByNamePerSecond) during(warmUpSeconds), constantUsersPerSec(getUserByNamePerSecond) during(durationSeconds))

    
    val scnloginUser = scenario("loginUserSimulation")
        .feed(loginUserQUERYFeeder)
        .exec(http("loginUser")
        .httpRequest("GET","/user/login")
        .queryParam("username","${username}")
        .queryParam("password","${password}")
        )

    // Run scnloginUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnloginUser.inject(rampUsersPerSec(1) to(loginUserPerSecond) during(warmUpSeconds), constantUsersPerSec(loginUserPerSecond) during(durationSeconds))

    
    val scnlogoutUser = scenario("logoutUserSimulation")
        .exec(http("logoutUser")
        .httpRequest("GET","/user/logout")
        )

    // Run scnlogoutUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnlogoutUser.inject(rampUsersPerSec(1) to(logoutUserPerSecond) during(warmUpSeconds), constantUsersPerSec(logoutUserPerSecond) during(durationSeconds))

    
    val scnupdateUser = scenario("updateUserSimulation")
        .feed(updateUserBodyFeeder)
        .exec(http("updateUser")
        .httpRequest("PUT","/user/${username}")
        .body(StringBody(User.toStringBody("${password}","${id}","${lastName}","${firstName}","${email}","${userStatus}","${phone}","${username}")))
        )

    // Run scnupdateUser with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnupdateUser.inject(rampUsersPerSec(1) to(updateUserPerSecond) during(warmUpSeconds), constantUsersPerSec(updateUserPerSecond) during(durationSeconds))



    setUp(
        scenarioBuilders.toList
    ).protocols(httpConf)

}
