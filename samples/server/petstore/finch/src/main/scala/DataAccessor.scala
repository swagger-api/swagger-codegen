package io.swagger

// TODO: properly handle custom imports
import java.io._
import java.util.Date
import java.util.UUID

import io.swagger.models._

trait DataAccessor {
    // TODO: apiInfo -> apis -> operations = ???
    // NOTE: ??? throws a not implemented exception

        /**
        * 
        * @return A Unit
        */
        def Pet_addPet(body: Pet): Unit = ???

        /**
        * 
        * @return A Unit
        */
        def Pet_deletePet(petId: Long, apiKey: Option[String]): Unit = ???

        /**
        * 
        * @return A Seq[Pet]
        */
        def Pet_findPetsByStatus(status: Seq[String]): Seq[Pet] = ???

        /**
        * 
        * @return A Seq[Pet]
        */
        def Pet_findPetsByTags(tags: Seq[String]): Seq[Pet] = ???

        /**
        * 
        * @return A Pet
        */
        def Pet_getPetById(petId: Long, authParamapi_key: String): Pet = ???

        /**
        * 
        * @return A Unit
        */
        def Pet_updatePet(body: Pet): Unit = ???

        /**
        * 
        * @return A Unit
        */
        def Pet_updatePetWithForm(petId: Long, name: Option[String], status: Option[String]): Unit = ???

        /**
        * 
        * @return A ApiResponse
        */
        def Pet_uploadFile(petId: Long, additionalMetadata: Option[String], file: FileUpload): ApiResponse = ???

        /**
        * 
        * @return A Unit
        */
        def Store_deleteOrder(orderId: String): Unit = ???

        /**
        * 
        * @return A Map[String, Int]
        */
        def Store_getInventory(authParamapi_key: String): Map[String, Int] = ???

        /**
        * 
        * @return A Order
        */
        def Store_getOrderById(orderId: Long): Order = ???

        /**
        * 
        * @return A Order
        */
        def Store_placeOrder(body: Order): Order = ???

        /**
        * 
        * @return A Unit
        */
        def User_createUser(body: User): Unit = ???

        /**
        * 
        * @return A Unit
        */
        def User_createUsersWithArrayInput(body: Seq[User]): Unit = ???

        /**
        * 
        * @return A Unit
        */
        def User_createUsersWithListInput(body: Seq[User]): Unit = ???

        /**
        * 
        * @return A Unit
        */
        def User_deleteUser(username: String): Unit = ???

        /**
        * 
        * @return A User
        */
        def User_getUserByName(username: String): User = ???

        /**
        * 
        * @return A String
        */
        def User_loginUser(username: String, password: String): String = ???

        /**
        * 
        * @return A Unit
        */
        def User_logoutUser(): Unit = ???

        /**
        * 
        * @return A Unit
        */
        def User_updateUser(username: String, body: User): Unit = ???

}