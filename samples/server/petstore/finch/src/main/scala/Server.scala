package io.swagger.petstore

import com.twitter.finagle.Http
import com.twitter.finagle.util.LoadService
import com.twitter.util.{Await, Future}


class Server {
  // Loads implementation defined in resources/META-INF/services/io.swagger.petstore.DataAccessor
  val db = LoadService[DataAccessor]() match {
    case accessor :: _ => accessor
    case _ => new DataAccessor { }
  }

  val service = endpoint.makeService(db)

  val server = Http.serve(":8080", service) //creates service

  Await.ready(server)

  def close(): Future[Unit] = {
    Await.ready(server.close())
  }
}

/**
 * Launches the PetstoreAPI service when the system is ready.
 */
object Server extends Server with App {
  Await.ready(server)
}
