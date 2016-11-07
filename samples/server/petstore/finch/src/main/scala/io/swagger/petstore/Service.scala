package io.swagger.petstore

import com.twitter.finagle.Http
import com.twitter.util.{Await, Future}

class PetstoreDb {

}
/**
  * Created by jim on 11/6/16.
  */
class Server {
  val db = new PetstoreDb()
  val service = endpoint.makeService(db)

  val server = Http.serve(":8080", service) //creates service

  Await.ready(server)

  def close(): Future[Unit] = {
    Await.ready(server.close())
  }

}

object Server extends Server with App {
  Await.ready(server)
}