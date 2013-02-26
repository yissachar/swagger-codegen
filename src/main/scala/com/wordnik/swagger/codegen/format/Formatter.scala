package com.wordnik.swagger.codegen.format

import com.wordnik.swagger.model.{ModelProperty, Operation}
import com.wordnik.swagger.codegen.config.LanguageConfig

case class ModelPropertyTemplateData(
  name: String,
  nameSingular: String,
  baseType: String,
  baseTypeVarName: String,
  baseName: String,
  datatype: String,
  defaultValue: String,
  description: String,
  notes: String,
  required: Boolean,
  getter: String,
  setter: String,
  isList: Boolean,
  isMap: Boolean,
  isContainer: Boolean,
  isNotContainer: Boolean,
  isPrimitive: Boolean,
  complexType: String,
  hasMore: Boolean = true
)
case class ModelTemplateData(
  classname: String,
  classVarName: String,
  modelPackage: Option[String],
  newLine: String,
  vars: List[ModelPropertyTemplateData],
  imports: Set[String],
  hasMore: Boolean = true
)
case class ParameterTemplateData(
        `type`: String,
        defaultValue: Option[String],
        dataType: String,
        swaggerDataType: String,
        description: String,
        allowMultiple: Boolean,
        allowableValues: Option[String],
        optional: Boolean,
        required: Boolean,
        paramName: String,
        baseName: String,
        secondaryParam: Boolean = true,
        hasMore: Boolean = true) {
  val headerParameter = `type`.toUpperCase == "HEADER"
  val pathParameter = `type`.toUpperCase == "PATH"
  val queryParameter = `type`.toUpperCase == "QUERY"
  val bodyParameter = `type`.toUpperCase == "BODY"
}

case class RequiredParameterTemplateData(
  paramName: String,
  defaultValue: Option[String],
  baseName: String,
  hasMore: Boolean = true
)

case class OperationTemplateData(
  path: String,
  nickname: String,
  summary: String,
  notes: String,
  deprecated: Boolean,
  bodyParam: Option[String],
  allParams: List[ParameterTemplateData],
  bodyParams: List[ParameterTemplateData],
  pathParams: List[ParameterTemplateData],
  queryParams: List[ParameterTemplateData],
  headerParams: List[ParameterTemplateData],
  requiredParams: List[RequiredParameterTemplateData],
  httpMethod: String,
  requiredParamCount: Int,
  returnType: Option[String],
  returnBaseType: Option[String],
  returnContainer: String,
  returnSimpleType: Boolean,
  returnTypeIsPrimitive: Boolean
) {
  val get = httpMethod.toUpperCase == "GET"
  val post = httpMethod.toUpperCase == "POST"
  val put = httpMethod.toUpperCase == "PUT"
  val delete = httpMethod.toUpperCase == "DELETE"
  val head = httpMethod.toUpperCase == "HEAD"
  val options = httpMethod.toUpperCase == "OPTIONS"
  val patch = httpMethod.toUpperCase == "PATCH"
}
case class ApiModelTemplateData(model: List[ModelTemplateData])
case class TemplateData (
  name: String,
  `package`: String,
  baseName: String,
  className: String,
  invokerPackage: String,
  imports: List[Map[String, String]],
  requiredModels: List[Map[String, String]],
  operations: List[OperationTemplateData],
  models: ApiModelTemplateData,
  basePath: String
)

trait Formatter extends FilePaths {
  def languageConfig: LanguageConfig

  def escapeReservedWord(word: String) = word

  def processApiMap(m: OperationTemplateData): OperationTemplateData = m
  def processModelMap(m: ModelTemplateData): ModelTemplateData = m

  // method name from operation.nickname
  def methodName(name: String): String = name

  def processOperation(apiPath: String, op: Operation): Operation = op

  def processResponseClass(responseClass: String): Option[String] = Some(responseClass)

  def processApiOperation(apiPath: String, op: Operation):Operation = op
  def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(languageConfig.typeMapping.getOrElse(e, e))
    }
  }

  // mapping for datatypes
  def declaration(property: ModelProperty) = {
    val t = declaredType(property.`type`)
    val v = defaultValue(t, property)
    (t, v)
  }

  def declaredType(dataType: String): String = {
    languageConfig.typeMapping.getOrElse(dataType, dataType)
  }

  def getter(name: String, datatype: String) = {
    val base = datatype match {
      case "boolean" => "is"
      case _ => "get"
    }
    base + name(0).toUpper + name.substring(1)
  }

  def setter(name: String, datatype: String) = {
    val base = datatype match {
      case _ => "set"
    }
    base + name(0).toUpper + name.substring(1)
  }

  def varName(name: String): String = {
    name match {
      case _ if (languageConfig.reservedWords.contains(name)) => escapeReservedWord(name)
      case _ => languageConfig.typeMapping.getOrElse(name, name)
    }
  }

  def defaultValue(datatype: String, v: String): Option[String] = {
    if (v != "" && v != null) {
      datatype match {
        case "int" => Some(v)
        case "long" => Some(v)
        case "double" => Some(v)
        case x if x == "string" || x == "String" => {
          v match {
            case e: String => Some("\"" + v + "\"")
            case _ => None
          }
        }
        case _ => None
      }
    } else None
  }

  def defaultValue(dataType: String, obj: ModelProperty) = {
    dataType match {
      case "int" => "0"
      case "long" => "0L"
      case "float" => "0f"
      case "double" => "0.0"
      case e: String if (Set("List").contains(e)) => {
        val inner = {
          obj.items map (it => it.ref getOrElse it.`type`) getOrElse (throw new Exception("no inner type defined"))
        }
        "new java.util.ArrayList[" + declaredType(inner) + "]" + "()"
      }
      case _ => "_"
    }
  }
}