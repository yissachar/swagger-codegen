package com.wordnik.swagger

import java.io.File
import model._

package object codegen {
  type Phase[F, S] = F => S
  type Compiler = CompilerConfig => Seq[File]
  type FileGenerator[T <: GeneratorContext] = T => Seq[File]
  type BundleContext = (GeneratorConfig, ResourceListing, List[ApiListing])


  val DefaultCompiler: Compiler =
    ApiCompiler(FetchListings(), CreateBundles(), BuildTemplateContext() andThen GenerateFiles())

  object ApiCompiler {
    def apply(
      fetchListings: Phase[CompilerConfig, BundleContext],
      createBundles: Phase[BundleContext, GeneratorContext],
      generateFiles: FileGenerator[GeneratorContext]): Compiler =
        fetchListings andThen createBundles andThen generateFiles
  }

  object lang {
    val Scala =
      CompilerConfig(
        packageName = "com.wordnik.client",
        templateDir = "scala",
        destinationDir = "generated-code/scala/src/main/scala",
        apiTemplateFiles = Map("api.mustache" -> ".scala"),
        modelTemplateFiles = Map("model.mustache" -> ".scala"),
        formatter = new Formatter {
          val languageConfig: LanguageConfig =
            LanguageConfig(
              defaultIncludes = Set("Int", "String", "Long", "Float", "Double", "Boolean", "Any"),
              languageSpecificPrimitives = Set.empty,
              typeMapping = Map(
                "boolean" -> "Boolean",
                "string" -> "String",
                "int" -> "Int",
                "float" -> "Float",
                "long" -> "Long",
                "double" -> "Double",
                "file" -> "File",
                "object" -> "Any"),
              reservedWords = Set("type", "package", "match", "object"),
              importMapping = Map("Date" -> "java.util.Date", "File" -> "java.io.File"),
              invokerPackage = Some("com.wordnik.client.common"),
              apiPackage = Some("com.wordnik.client.api"),
              modelPackage = Some("com.wordnik.client.model"),
              supportingFiles = List.empty
            )

          override def processResponseClass(responseClass: String): Option[String] =
            if (responseClass.toUpperCase == "VOID") None
            else Some(languageConfig.typeMapping.getOrElse(responseClass, responseClass))

          override def declaration(property: ModelProperty): (String, String) = {
            property.`type` match {
              case "Array" => {
                property.items map { items =>
                  val inner = items.ref.getOrElse(items.`type`)
                  ("List[%s]" format declaredType(inner), defaultValue(inner, property))
                } getOrElse sys.error("No inner type defined.")
              }
              case e => (declaredType(e), defaultValue(e, property))
            }
          }

          override def escapeReservedWord(word: String): String = "`" + word + "`"
        })
  }
}