import com.wordnik.petstore.api._

object Simple {

  def main(args: Array[String]) = {
    val api = new PetApi
    println(api.getPetById("1"))
    exit(0)
  }
}