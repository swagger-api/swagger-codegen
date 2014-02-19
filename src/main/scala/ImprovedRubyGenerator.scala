import com.wordnik.swagger.codegen.BasicRubyGenerator

object ImprovedRubyGenerator extends BasicRubyGenerator {
  def main(args: Array[String]) = generateClient(args)

  //fix bad syntax in class name name was {some_name}_api
  override def toApiName(name: String) = {
    var fixedName = name.replaceAll("(\\{|\\})","")
    fixedName(0).toUpper + fixedName.substring(1) + "_api"
  }

  //fix bad syntax on class name
  override def toModelFilename(name: String) = name.toLowerCase.replaceAll("(\\{|\\})","")

  //fix bad syntax on filename
  override def toApiFilename(name: String) = name.toLowerCase.replaceAll("(\\{|\\})","") + "_api"

}
