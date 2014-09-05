package com.wordnik.swagger.codegen

import scala.collection.mutable
import java.io.{File, FileWriter}
import com.wordnik.swagger.codegen.model._
import scala.collection.mutable.{HashMap, ListBuffer}
import language.CodegenConfig
import scala.io.Source
import org.json4s.jackson.Serialization._
import org.fusesource.scalate.{Template, TemplateSource, TemplateEngine}
import org.apache.commons.io.FileUtils
import com.wordnik.swagger.codegen.util.{CoreUtils, ApiExtractor, ResourceExtractor}
import com.wordnik.swagger.codegen.spec.SwaggerSpecValidator
import mojolly.inflector.InflectorImports._
import org.rogach.scallop.{ScallopConf, Scallop}
import scala.annotation.switch
import scala.collection.JavaConverters._

case class SwaggerApi(
             clientName: String,
             resourceUrl: String,
             packageName: String,
             apiTemplates: Map[String, String] = Map("api.mustache" -> ".scala"),
             modelTemplates: Map[String, String] = Map("model.mustache" -> ".scala"),
             apiKey: Option[String] = None,
             baseUrl: Option[String] = None,
             excludedApis: Set[String] = Set.empty,
             excludedModels: Set[String] = Set.empty,
             excludedModelPackages: Set[String] = Set.empty,
             defaultImports: Map[String, String] = Map.empty)
case class SwaggerGenConfig(
             api: SwaggerApi,
             templateDir: File,
             codeDir: File,
             projectRoot: File,
             defaultIncludes: Set[String] = Set.empty,
             typeMapping: Map[String, String] = Map.empty,
             defaultImports: Map[String, String] = Map.empty,
             excludedModelPackages: Set[String] = Set.empty)
object AsycnClientGeneratorConf {
  val appBanner: String = """
        |
        |
        |  .--.--.
        | /  /    '.
        ||  :  /`. /         .---.                                        __  ,-.
        |;  |  |--`         /. ./|           ,----._,. ,----._,.        ,' ,'/ /|
        ||  :  ;_        .-'-. ' | ,--.--.  /   /  ' //   /  ' /  ,---. '  | |' |
        | \  \    `.    /___/ \: |/       \|   :     |   :     | /     \|  |   ,'
        |  `----.   \.-'.. '   ' .--.  .-. |   | .\  |   | .\  ./    /  '  :  /
        |  __ \  \  /___/ \:     '\__\/: . .   ; ';  .   ; ';  .    ' / |  | '
        | /  /`--'  .   \  ' .\   ," .--.; '   .   . '   .   . '   ;   /;  : |
        |'--'.     / \   \   ' \ /  /  ,.  |`---`-'| |`---`-'| '   |  / |  , ;
        |  `--'---'   \   \  |--;  :   .'   .'__/\_: |.'__/\_: |   :    |---'
        |              \   \ |  |  ,     .-.|   :    :|   :    :\   \  /
        |               '---"    `--`---'    \   \  /  \   \  /  `----'
        |                                     `--`-'    `--`-'
        |
        |         Swagger Codegen, Reverb Technologies Inc. (c) 2009-2013
        |      For more info, visit: https://developers.helloreverb.com/swagger/
      """.stripMargin
}
class AsycnClientGeneratorConf(arguments: Seq[String]) extends ScallopConf(arguments) {

  val name = opt[String](required = true, descr = "The name of the generated client.")
  val `package` = opt[String](default = Some("com.wordnik.swagger.client.async"), descr = "The package for the generated code.")
  val resourceUrl = trailArg[String](descr = "The url to use for fetching the swagger spec from. This can be a http(s) url or a file path.")
  val baseUrl = opt[String](descr = "The url to use when you want to override the base url provided by the resource url json.")
  val apiKey = opt[String](required = false, descr = "An optional api key to use when calling the swagger api")
  val templateDir = opt[String](descr = "The directory that contains the templates for use in this generator", default = Some("asyncscala"))
  val codeDir = opt[String](descr = "The directory to use as base for generating code files, this will contain the generated scala files.", default = Some("src/main/scala"), hidden = true)
  val projectRoot = opt[String](descr = "The directory to use as project dir, this will receive the build files (*.sbt, *.pom)", default = Some("."))

  mainOptions = Seq(resourceUrl, name)

  banner("""
           |Usage: scala-async.sh [OPTION] spec-url
           |
           |The scala-async tool generates a swagger api client, using async-http-client
           |and stdlib futures.
           |
           |Options:
           |
         """.stripMargin)

  footer("\nFor more information, visit https://developers.helloreverb.com/swagger/")
}

object ScalaAsyncClientGenerator extends App {
  val appBanner: String = AsycnClientGeneratorConf.appBanner

  val opts = new AsycnClientGeneratorConf(if (args.nonEmpty) args else Array("--help"))
  val rootDir = new File(opts.projectRoot())
  val codeDir = {
    val cd = opts.codeDir()
    if (cd.startsWith("/")) new File(cd)
    else new File(rootDir, cd)
  }
  val resUrl = {
    val r = opts.resourceUrl()
    if (!r.startsWith("http") && !r.startsWith("file")) sys.props("fileMap") = r
    r
  }
  val baseUrl = opts.baseUrl.get
  val cfg = SwaggerGenConfig(
    api = SwaggerApi(opts.name(), resUrl, opts.`package`(), apiKey = opts.apiKey.get, baseUrl = baseUrl),
    templateDir = new File(opts.templateDir()),
    codeDir = new File(rootDir, opts.codeDir()),
    projectRoot = rootDir
  )

  val generator = new ScalaAsyncClientGenerator(cfg)

  val clientOpts = new ClientOpts()
  val props = new HashMap[String, String]
  if(resUrl.startsWith("http"))
    clientOpts.uri = resUrl
  else
    props += "fileMap" -> resUrl

  props += "clientName" -> cfg.api.clientName.underscore.pascalize
  
  clientOpts.properties = props.toMap.asJava

  println(appBanner)
  generator.generate(clientOpts)
}

class ScalaAsyncClientGenerator(cfg: SwaggerGenConfig) extends BasicGenerator {
  private[this] val pascalizedClientName = cfg.api.clientName.underscore.pascalize

  override val packageName: String = cfg.api.packageName
  override val templateDir: String = cfg.templateDir.getPath
  override val destinationDir: String = cfg.codeDir.getPath
  override val fileSuffix: String = ".scala"
  override val modelPackage: Option[String] = Some(packageName + ".model")
  override val apiPackage: Option[String] = Some(packageName + ".apis")

  additionalParams += "clientName" -> cfg.api.clientName.underscore.pascalize
  additionalParams += "projectName" -> cfg.api.clientName.underscore.dasherize


  override val reservedWords: Set[String] =
    Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield")
  override val importMapping: Map[String, String] = Map(
      "Set" -> "scala.collection.immutable.Set",
      "Date" -> "java.util.Date",
      "DateTime" -> "org.joda.time.DateTime",
      "DateTimeZone" -> "org.joda.time.DateTimeZone",
      "Chronology" -> "org.joda.time.Chronology",
      "File" -> "java.io.File"
    ) ++ cfg.defaultImports ++ cfg.api.defaultImports
  override val typeMapping = Map(
      "array" -> "List",
      "boolean" -> "Boolean",
      "string" -> "String",
      "int" -> "Int",
      "long" -> "Long",
      "float" -> "Float",
      "byte" -> "Byte",
      "short" -> "Short",
      "char" -> "Char",
      "long" -> "Long",
      "double" -> "Double",
      "object" -> "Any",
      "file" -> "File") ++ cfg.typeMapping

  override val defaultIncludes = Set(
      "Int",
      "String",
      "Long",
      "Short",
      "Char",
      "Byte",
      "Float",
      "Double",
      "Boolean",
      "AnyRef",
      "Any") ++  cfg.defaultIncludes ++ cfg.api.excludedModels

  override def supportingFiles = List(
    ("client.mustache", destinationDir + "/" + cfg.api.packageName.replace('.', '/'), (pascalizedClientName +".scala")),
    ("sbt.mustache", cfg.projectRoot.getPath, "swagger-client.sbt")
  )

  modelTemplateFiles ++= cfg.api.modelTemplates
  apiTemplateFiles ++= cfg.api.apiTemplates

  codegen = new Codegen(this)

  override def getBasePath(host: String, basePath: String, fileMap: Option[String]): String =
    cfg.api.baseUrl.getOrElse(super.getBasePath(host, basePath, fileMap))

  override def toApiName(name: String) = {
    name.replaceAll("\\{","").replaceAll("\\}", "") match {
      case s: String if(s.length > 0) => s.underscore.pascalize + "Client"
      case _ => "Client"
    }
  }

  // response classes--if you don't want a response class, override and set to None
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None //Some("Unit")
      case e: String => Some(toDeclaredType(e))
    }
  }

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None //Some("Unit")
      case e: String => Some(toDeclaredType(e))
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = (dt.indexOf("[")) match {
      case -1 => dt
      case n: Int => {
        if (dt.substring(0, n).toLowerCase == "array") {
          val dtt = dt.substring(n + 1, dt.length - 1)
          "List[%s]".format(typeMapping.getOrElse(dtt, dtt))
        } else dt
      }
    }
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: ModelProperty): (String, String) = {
    obj.`type` match {
      case "Array" | "array" =>  makeContainerType(obj, "List")
      case "Set" | "set" => makeContainerType(obj, "Set")
      case e: String => (toDeclaredType(e), toDefaultValue(e, obj))
    }
  }

  private def makeContainerType(obj: ModelProperty, container: String): (String, String) = {
    val inner = {
      obj.items match {
        case Some(items) => items.ref.getOrElse(items.`type`)
        case _ => throw new Exception("no inner type defined")
      }
    }
    val e = "%s[%s]" format (container, toDeclaredType(inner))
    (e, toDefaultValue(inner, obj))
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"

}
