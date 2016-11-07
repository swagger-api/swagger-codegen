package io.swagger.petstore

//import User

import _root_.argonaut._, Argonaut._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.FileUpload
import com.twitter.util.Future
import argonaut.Argonaut._
import io.finch._, items._
import io.finch.argonaut._
import java.io.File

object UserApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    public def endpoints(da: DataAccessor) =
            createUser(da) :+:
            createUsersWithArrayInput(da) :+:
            createUsersWithListInput(da) :+:
            deleteUser(da) :+:
            getUserByName(da) :+:
            loginUser(da) :+:
            logoutUser(da) :+:
            updateUser(da)

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def createUser(da: DataAccessor): Endpoint[Unit] =
            post(  User) { (body: User) => 
                NoContent(da.User_createUser(body))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def createUsersWithArrayInput(da: DataAccessor): Endpoint[Unit] =
            post(  List[User]) { (body: List[User]) => 
                NoContent(da.User_createUsersWithArrayInput(body))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def createUsersWithListInput(da: DataAccessor): Endpoint[Unit] =
            post(  List[User]) { (body: List[User]) => 
                NoContent(da.User_createUsersWithListInput(body))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def deleteUser(da: DataAccessor): Endpoint[Unit] =
            post(  ) { (username: String) => 
                NoContent(da.User_deleteUser(username))
        }

        /**
        * 
        * @return And endpoint representing a User
        */
        private def getUserByName(da: DataAccessor): Endpoint[User] =
            post(  ) { (username: String) => 
                Ok(da.User_getUserByName(username))
        }

        /**
        * 
        * @return And endpoint representing a String
        */
        private def loginUser(da: DataAccessor): Endpoint[String] =
            post(  String ? String) { (username: String, password: String) => 
                Ok(da.User_loginUser(username, password))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def logoutUser(da: DataAccessor): Endpoint[Unit] =
            post(  ) { 
                NoContent(da.User_logoutUser())
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updateUser(da: DataAccessor): Endpoint[Unit] =
            post(  User) { (username: String, body: User) => 
                NoContent(da.User_updateUser(username, body))
        }

}
