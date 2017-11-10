package io.swagger.client.api

import io.swagger.client.model._
import com.typesafe.config.ConfigFactory

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.structure.PopulationBuilder

import java.io.File

import scala.collection.mutable

class PetApiSimulation extends Simulation {

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
    val addPetPerSecond = config.getDouble("performance.operationsPerSecond.addPet")
    val deletePetPerSecond = config.getDouble("performance.operationsPerSecond.deletePet")
    val findPetsByStatusPerSecond = config.getDouble("performance.operationsPerSecond.findPetsByStatus")
    val findPetsByTagsPerSecond = config.getDouble("performance.operationsPerSecond.findPetsByTags")
    val getPetByIdPerSecond = config.getDouble("performance.operationsPerSecond.getPetById")
    val updatePetPerSecond = config.getDouble("performance.operationsPerSecond.updatePet")
    val updatePetWithFormPerSecond = config.getDouble("performance.operationsPerSecond.updatePetWithForm")
    val uploadFilePerSecond = config.getDouble("performance.operationsPerSecond.uploadFile")

    val scenarioBuilders: mutable.MutableList[PopulationBuilder] = new mutable.MutableList[PopulationBuilder]()

    // Set up CSV feeders
    val addPetBodyFeeder = csv(userDataDirectory + File.separator + "addPet-bodyParams.csv", escapeChar = '\\').random
    val deletePetHEADERFeeder = csv(userDataDirectory + File.separator + "deletePet-headerParams.csv").random
    val deletePetPATHFeeder = csv(userDataDirectory + File.separator + "deletePet-pathParams.csv").random
    val findPetsByStatusQUERYFeeder = csv(userDataDirectory + File.separator + "findPetsByStatus-queryParams.csv").random
    val findPetsByTagsQUERYFeeder = csv(userDataDirectory + File.separator + "findPetsByTags-queryParams.csv").random
    val getPetByIdPATHFeeder = csv(userDataDirectory + File.separator + "getPetById-pathParams.csv").random
    val updatePetBodyFeeder = csv(userDataDirectory + File.separator + "updatePet-bodyParams.csv", escapeChar = '\\').random
    val updatePetWithFormFORMFeeder = csv(userDataDirectory + File.separator + "updatePetWithForm-formParams.csv").random
    val updatePetWithFormPATHFeeder = csv(userDataDirectory + File.separator + "updatePetWithForm-pathParams.csv").random
    val uploadFileFORMFeeder = csv(userDataDirectory + File.separator + "uploadFile-formParams.csv").random
    val uploadFilePATHFeeder = csv(userDataDirectory + File.separator + "uploadFile-pathParams.csv").random

    // Setup all scenarios

    
    val scnaddPet = scenario("addPetSimulation")
        .feed(addPetBodyFeeder)
        .exec(http("addPet")
        .httpRequest("POST","/pet")
        .body(StringBody(Pet.toStringBody("${id}","${category}","${name}","${tags}","${status}","${photoUrls}")))
        )

    // Run scnaddPet with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnaddPet.inject(rampUsersPerSec(1) to(addPetPerSecond) during(warmUpSeconds), constantUsersPerSec(addPetPerSecond) during(durationSeconds))

    
    val scndeletePet = scenario("deletePetSimulation")
        .feed(deletePetHEADERFeeder)
        .exec(http("deletePet")
        .httpRequest("DELETE","/pet/${petId}")
        .header("api_key","${api_key}")
        )

    // Run scndeletePet with warm up and reach a constant rate for entire duration
    scenarioBuilders += scndeletePet.inject(rampUsersPerSec(1) to(deletePetPerSecond) during(warmUpSeconds), constantUsersPerSec(deletePetPerSecond) during(durationSeconds))

    
    val scnfindPetsByStatus = scenario("findPetsByStatusSimulation")
        .feed(findPetsByStatusQUERYFeeder)
        .exec(http("findPetsByStatus")
        .httpRequest("GET","/pet/findByStatus")
        .queryParam("status","${status}")
        )

    // Run scnfindPetsByStatus with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnfindPetsByStatus.inject(rampUsersPerSec(1) to(findPetsByStatusPerSecond) during(warmUpSeconds), constantUsersPerSec(findPetsByStatusPerSecond) during(durationSeconds))

    
    val scnfindPetsByTags = scenario("findPetsByTagsSimulation")
        .feed(findPetsByTagsQUERYFeeder)
        .exec(http("findPetsByTags")
        .httpRequest("GET","/pet/findByTags")
        .queryParam("tags","${tags}")
        )

    // Run scnfindPetsByTags with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnfindPetsByTags.inject(rampUsersPerSec(1) to(findPetsByTagsPerSecond) during(warmUpSeconds), constantUsersPerSec(findPetsByTagsPerSecond) during(durationSeconds))

    
    val scngetPetById = scenario("getPetByIdSimulation")
        .exec(http("getPetById")
        .httpRequest("GET","/pet/${petId}")
        )

    // Run scngetPetById with warm up and reach a constant rate for entire duration
    scenarioBuilders += scngetPetById.inject(rampUsersPerSec(1) to(getPetByIdPerSecond) during(warmUpSeconds), constantUsersPerSec(getPetByIdPerSecond) during(durationSeconds))

    
    val scnupdatePet = scenario("updatePetSimulation")
        .feed(updatePetBodyFeeder)
        .exec(http("updatePet")
        .httpRequest("PUT","/pet")
        .body(StringBody(Pet.toStringBody("${id}","${category}","${name}","${tags}","${status}","${photoUrls}")))
        )

    // Run scnupdatePet with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnupdatePet.inject(rampUsersPerSec(1) to(updatePetPerSecond) during(warmUpSeconds), constantUsersPerSec(updatePetPerSecond) during(durationSeconds))

    
    val scnupdatePetWithForm = scenario("updatePetWithFormSimulation")
        .feed(updatePetWithFormFORMFeeder)
        .exec(http("updatePetWithForm")
        .httpRequest("POST","/pet/${petId}")
        .formParam("name","${name}")
        .formParam("status","${status}")
        )

    // Run scnupdatePetWithForm with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnupdatePetWithForm.inject(rampUsersPerSec(1) to(updatePetWithFormPerSecond) during(warmUpSeconds), constantUsersPerSec(updatePetWithFormPerSecond) during(durationSeconds))

    
    val scnuploadFile = scenario("uploadFileSimulation")
        .feed(uploadFileFORMFeeder)
        .exec(http("uploadFile")
        .httpRequest("POST","/pet/${petId}/uploadImage")
        .formParam("file","${file}")
        .formParam("additionalMetadata","${additionalMetadata}")
        )

    // Run scnuploadFile with warm up and reach a constant rate for entire duration
    scenarioBuilders += scnuploadFile.inject(rampUsersPerSec(1) to(uploadFilePerSecond) during(warmUpSeconds), constantUsersPerSec(uploadFilePerSecond) during(durationSeconds))



    setUp(
        scenarioBuilders.toList
    ).protocols(httpConf)

}
