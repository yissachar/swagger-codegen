import com.wordnik.swagger.codegen.BasicGenerator

object ObjcPetstoreCodegen extends ObjcCodegen {
  def main(args: Array[String]) = generateClient(args)

  // where to write generated code
  override def destinationDir = "samples/objc/"

  // supporting classes
  override def supportingFiles =
    List(
      ("SwaggerObject.h", destinationDir, "SwaggerObject.h"),
      ("SwaggerObject.m", destinationDir, "SwaggerObject.m"),
      ("ApiInvoker.h", destinationDir, "ApiInvoker.h"),
      ("ApiInvoker.m", destinationDir, "ApiInvoker.m"),
      ("Date.h", destinationDir, "Date.h"),
      ("Date.m", destinationDir, "Date.m"))
}
