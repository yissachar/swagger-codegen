package com.wordnik.swagger
package codegen

import mojolly.inflector.InflectorImports._
import model._

trait FilePaths {
  def resourcePath(host: String) = sys.props get "fileMap" map (_ + "/resources.json") getOrElse host

  def basePath(basePath: String) = sys.props get "fileMap" getOrElse basePath

  def modelName(name: String) = name.pascalize

  def apiName(name: String) = {
    name.replaceAll("\\{","").replaceAll("\\}", "") match {
      case s: String if(s.length > 0) => s(0).toUpper + s.substring(1) + "Api"
      case _ => "Api"
    }
  }

  def nameFromPath(apiPath: String) = {
    apiPath.split("/")(1).split("\\.")(0).replaceAll("/", "")
  }

  def apiNameFromPath(apiPath: String) = apiName(nameFromPath(apiPath))

  def resourceNameFromFullPath(apiPath: String) = {
    apiPath.split("/")(1).split("\\.")(0).replaceAll("/", "")
  }

  def modelFilename(name: String) = modelName(name)
  def apiFilename(name: String) = apiName(name)
}

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