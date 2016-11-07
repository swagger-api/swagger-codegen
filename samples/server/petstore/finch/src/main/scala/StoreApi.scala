package io.swagger.petstore

//import Order

import _root_.argonaut._, Argonaut._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.FileUpload
import com.twitter.util.Future
import argonaut.Argonaut._
import io.finch._, items._
import io.finch.argonaut._
import java.io.File

object StoreApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    public def endpoints(da: DataAccessor) =
            deleteOrder(da) :+:
            getInventory(da) :+:
            getOrderById(da) :+:
            placeOrder(da)

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def deleteOrder(da: DataAccessor): Endpoint[Unit] =
            post(  ) { (orderId: String) => 
                NoContent(da.Store_deleteOrder(orderId))
        }

        /**
        * 
        * @return And endpoint representing a Map[String, Int]
        */
        private def getInventory(da: DataAccessor): Endpoint[Map[String, Int]] =
            post(  ) { 
                Ok(da.Store_getInventory())
        }

        /**
        * 
        * @return And endpoint representing a Order
        */
        private def getOrderById(da: DataAccessor): Endpoint[Order] =
            post(  ) { (orderId: Long) => 
                Ok(da.Store_getOrderById(orderId))
        }

        /**
        * 
        * @return And endpoint representing a Order
        */
        private def placeOrder(da: DataAccessor): Endpoint[Order] =
            post(  Order) { (body: Order) => 
                Ok(da.Store_placeOrder(body))
        }

}
