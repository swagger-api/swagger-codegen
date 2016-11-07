package io.swagger.petstore

//import Pet
//import java.io.File
//import ApiResponse

import _root_.argonaut._, Argonaut._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.FileUpload
import com.twitter.util.Future
import argonaut.Argonaut._
import io.finch._, items._
import io.finch.argonaut._
import java.io.File

object PetApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    public def endpoints(da: DataAccessor) =
            addPet(da) :+:
            deletePet(da) :+:
            findPetsByStatus(da) :+:
            findPetsByTags(da) :+:
            getPetById(da) :+:
            updatePet(da) :+:
            updatePetWithForm(da) :+:
            uploadFile(da)

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def addPet(da: DataAccessor): Endpoint[Unit] =
            post(  Pet) { (body: Pet) => 
                NoContent(da.Pet_addPet(body))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def deletePet(da: DataAccessor): Endpoint[Unit] =
            post(  String) { (petId: Long, apiKey: String) => 
                NoContent(da.Pet_deletePet(petId, apiKey))
        }

        /**
        * 
        * @return And endpoint representing a List[Pet]
        */
        private def findPetsByStatus(da: DataAccessor): Endpoint[List[Pet]] =
            post(  List[String]) { (status: List[String]) => 
                Ok(da.Pet_findPetsByStatus(status))
        }

        /**
        * 
        * @return And endpoint representing a List[Pet]
        */
        private def findPetsByTags(da: DataAccessor): Endpoint[List[Pet]] =
            post(  List[String]) { (tags: List[String]) => 
                Ok(da.Pet_findPetsByTags(tags))
        }

        /**
        * 
        * @return And endpoint representing a Pet
        */
        private def getPetById(da: DataAccessor): Endpoint[Pet] =
            post(  ) { (petId: Long) => 
                Ok(da.Pet_getPetById(petId))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updatePet(da: DataAccessor): Endpoint[Unit] =
            post(  Pet) { (body: Pet) => 
                NoContent(da.Pet_updatePet(body))
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updatePetWithForm(da: DataAccessor): Endpoint[Unit] =
            post(  String ? String) { (petId: Long, name: String, status: String) => 
                NoContent(da.Pet_updatePetWithForm(petId, name, status))
        }

        /**
        * 
        * @return And endpoint representing a ApiResponse
        */
        private def uploadFile(da: DataAccessor): Endpoint[ApiResponse] =
            post(  String ? File) { (petId: Long, additionalMetadata: String, file: File) => 
                Ok(da.Pet_uploadFile(petId, additionalMetadata, file))
        }

}
