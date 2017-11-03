package io.swagger.apis

import java.io._
import java.util.Date
import io.swagger._
import io.swagger.models._
import io.swagger.models.ApiResponse
import java.io.File
import io.swagger.models.Pet
import io.finch.circe._
import io.circe.generic.semiauto._
import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.Service
import com.twitter.finagle.Http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart.{FileUpload, InMemoryFileUpload, OnDiskFileUpload}
import com.twitter.util.Future
import com.twitter.io.Buf
import io.finch._, items._
import java.io.File

object PetApi {
    /**
    * Compiles all service endpoints.
    * @return Bundled compilation of all service endpoints.
    */
    def endpoints(da: DataAccessor) =
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
        post("pet"  :: jsonBody[Pet]) { (body:Pet, authParampetstore_auth: String) => 
                da.Pet_addPet(body:Pet, authParampetstore_auth: String)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def deletePet(da: DataAccessor): Endpoint[Unit] =
        delete("pet" :: long  :: headerOption("api_key")) { (petId:Long, apiKey:Option[String], authParampetstore_auth: String) => 
                da.Pet_deletePet(petId:Long, apiKey:Option[String], authParampetstore_auth: String)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Seq[Pet]
        */
        private def findPetsByStatus(da: DataAccessor): Endpoint[Seq[Pet]] =
        get("pet" :: "findByStatus"  :: params("status")) { (status:Seq[String], authParampetstore_auth: String) => 
                Ok(
                da.Pet_findPetsByStatus(status:Seq[String], authParampetstore_auth: String)
                )
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Seq[Pet]
        */
        private def findPetsByTags(da: DataAccessor): Endpoint[Seq[Pet]] =
        get("pet" :: "findByTags"  :: params("tags")) { (tags:Seq[String], authParampetstore_auth: String) => 
                Ok(
                da.Pet_findPetsByTags(tags:Seq[String], authParampetstore_auth: String)
                )
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Pet
        */
        private def getPetById(da: DataAccessor): Endpoint[Pet] =
        get("pet" :: long  :: header("api_key")) { (petId:Long, authParamapi_key: String) => 
                Ok(
                da.Pet_getPetById(petId:Long, authParamapi_key: String)
                )
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updatePet(da: DataAccessor): Endpoint[Unit] =
        put("pet"  :: jsonBody[Pet]) { (body:Pet, authParampetstore_auth: String) => 
                da.Pet_updatePet(body:Pet, authParampetstore_auth: String)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a Unit
        */
        private def updatePetWithForm(da: DataAccessor): Endpoint[Unit] =
        post("pet" :: long  :: string :: string) { (petId:Long, name:Option[String], status:Option[String], authParampetstore_auth: String) => 
                da.Pet_updatePetWithForm(petId:Long, name:Option[String], status:Option[String], authParampetstore_auth: String)
                NoContent[Unit]
        } handle {
          case e: Exception => BadRequest(e)
        }

        /**
        * 
        * @return And endpoint representing a ApiResponse
        */
        private def uploadFile(da: DataAccessor): Endpoint[ApiResponse] =
        post("pet" :: long :: "uploadImage"  :: string :: fileUpload("file")) { (petId:Long, additionalMetadata:Option[String], file:FileUpload, authParampetstore_auth: String) => 
                Ok(
                da.Pet_uploadFile(petId:Long, additionalMetadata:Option[String], file:FileUpload, authParampetstore_auth: String)
                )
        } handle {
          case e: Exception => BadRequest(e)
        }


    implicit private def fileUploadToFile(fileUpload: FileUpload) : File = {
        fileUpload match {
            case upload: InMemoryFileUpload =>
                bytesToFile(Buf.ByteArray.Owned.extract(upload.content))
            case upload: OnDiskFileUpload =>
                upload.content
            case _ => null
        }
    }

    private def bytesToFile(input: Array[Byte]): java.io.File = {
        val file = File.createTempFile("tmpPetApi", null)
        val output = new FileOutputStream(file)
        output.write(input)
        file
    }

    // This assists in params(string) application (which must be Seq[A] in parameter list) when the param is used as a List[A] elsewhere.
    implicit def seqList[A](input: Seq[A]): List[A] = input.toList
}
