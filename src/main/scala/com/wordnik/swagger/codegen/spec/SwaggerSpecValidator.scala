/**
 *  Copyright 2012 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger
package codegen
package spec

import model._
import SwaggerSpec._
import util.CoreUtils

import collection.{immutable, mutable}
import org.fusesource.scalate.{ TemplateSource, TemplateEngine }
import java.io.{ FileWriter, File }
import org.slf4j.LoggerFactory

class SwaggerSpecValidator(private val doc: ResourceListing,
  private val apis: List[ApiListing],
  private val fix: Boolean = true) extends PathUtil {

  import ValidationMessage._

  private val validationMessages = mutable.ListBuffer.empty[ValidationMessage]

  private val logger = LoggerFactory.getLogger(classOf[SwaggerSpecValidator].getName)

  def validate() {
    checkRootProperties()

    apis.foreach(api => {
      fixSubDoc(api)

      if (api.models != null && api.models.nonEmpty) {
        fixReturnModels(api.models, apis)
        fixInputDataTypes(api.models, apis)
        fixModels(api.models)
      } else {
        logger.warn("No models found for listing " + api.basePath)
      }
    })

    validateResponseModels(apis)
    println("----------")
    println(this)
  }

  def validateResponseModels(subDocs: List[ApiListing]) {
    val validModelNames = CoreUtils.extractAllModels(subDocs).map(m => m._1).toSet
    val requiredModels = new mutable.HashSet[String]
     subDocs.foreach(subDoc => {
       if (subDoc.apis != null) {
	       subDoc.apis.foreach(api => {
          api.operations.foreach(op => {
            requiredModels += {
              val responseClass = op.responseClass
              responseClass.indexOf("[") match {
		            case i: Int if (i > 0) => {
                  CoreUtils.extractBasePartFromType(responseClass)
            		}
            		case _ => responseClass
              }
            }
          })
	      })
      }
    })
    val missingModels = requiredModels.toSet -- (validModelNames ++ primitives)

    if (missingModels.size > 0) println("missing models: " + missingModels)
  }

  def generateReport(host: String, outputFilename: Option[String]) {
    outputFilename match {
      case Some(o) => {
        val rootDir = new java.io.File(".")
        val engine = new TemplateEngine(Some(rootDir))
        val templateLocation = "validator" + File.separator + "index.mustache"
        val template = engine.compile(
          TemplateSource.fromText(templateLocation, io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(templateLocation)).mkString))
        val output = engine.layout(templateLocation, template, immutable.HashMap(
          "messages" -> validationMessages,
          "host" -> host,
          "basePath" -> doc.basePath,
          "swaggerVersion" -> doc.swaggerVersion,
          "apiVersion" -> doc.apiVersion))
        val fw = new FileWriter(o, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + o)

      }

      case None =>
        println("Output file location not passed as program argument")
    }

  }

  /**
   * Checks the swagger.version, basePath and api.version which is
   * expected to be present in root resource listing
   *
   */
  private def checkRootProperties() {
    doc.swaggerVersion match {
      case e: String => println("swagger version: " + e)
      case _ => addError(doc, RESOURCE_LISTING, "Properties", "Missing swagger version")
    }
    doc.basePath match {
      case e: String => println("basePath: " + e)
      case _ => addError(doc, RESOURCE_LISTING, "Properties", "Missing base path")
    }
    doc.apiVersion match {
      case e: String => println("api version: " + e)
      case _ => addError(doc, RESOURCE_LISTING, "Properties", "Missing api version", WARNING)
    }
  }

  /**
   * this is here because sub documents don't have the same resourcePath as declared in
   * the main resource listing
   */
  private def fixSubDoc(api: ApiListing) {
    if (api.resourcePath.indexOf(".{format}") == -1) {
      doc.apis.foreach(op => {
        if (op.path.indexOf(".{format}") > 0 && op.path.replaceAll(".\\{format\\}", "") == api.resourcePath) {
          if (fix) {
            api.resourcePath = api.resourcePath + ".{format}"
          }
        }
      })
    }
  }

  /**
   * this is here because models don't have the proper references to types
   */
  private def fixModels(models: Map[String, Model]) {
    val validModelNames = models.map(_._1).toSet
    logger.debug("all valid models: " + validModelNames)
    for ((name, model) <- models) {
      // id of model
      getUpdatedType(validModelNames, model.id) match {
        case Some(updatedType) => {
          if (!model.id.equals(updatedType)) {
            addError(model, MODEL, model.id, "Invalid id. Best guess: %s" format updatedType)
            logger.debug("updated " + model.id + " to " + updatedType)
            if (fix) model.id = updatedType
          }
        }
        case None => {
          logger.debug("can't find type for " + model.name + ", type " + model.id)
          addError(model, MODEL, model.name, "Missing type (%s)" format model.id)
        }
      }

      model.properties.foreach(prop => {
        val subObjectName = prop._1
        val subObject = prop._2

        if (containers.contains(subObject.`type`)) {
          // process the sub object
          subObject.items match {
            case Some(item) => {
              getUpdatedType(validModelNames, item.ref.getOrElse(null)) match {
                case Some(updatedType) => {
                  if (!item.ref.get.equals(updatedType)) {
                    addError(model, MODEL_PROPERTY, "%s->%s: %s" format (model.id, subObjectName, subObject.`type`), "Invalid ref (%s). Best guess: %s" format (item.ref, updatedType))
                    logger.debug("updated subObject.items.ref " + item.ref + " to " + updatedType)
                    if (fix) {
                      subObject.items = Some(ModelRef(null, Some(updatedType)))
                    }
                  }
                }
                case None =>
              }
            }
            case _ =>
          }
        } else if (containers.contains(subObject.`type`)) {
          // process the sub object
          if (subObject.items != null && subObject.items != None && subObject.items.get.ref != null){
            subObject.items match {
              case Some(item) => {
                getUpdatedType(validModelNames, item.ref.getOrElse(null)) match {
                  case Some(updatedType) => {
                    if (!item.ref.equals(updatedType)) {
                      addError(model, MODEL_PROPERTY, "%s->%s: %s" format (model.id, subObjectName, subObject.`type`), "Invalid ref (%s). Best guess: %s" format (item.ref, updatedType))
                      logger.debug("updated subObject.items.ref " + item.ref + " to " + updatedType)
                      if (fix) subObject.items = Some(ModelRef(null, Some(updatedType)))
                    }
                  }
                  case None => {
                    addError(model, MODEL_PROPERTY, "%s->%s: %s" format (model.id, subObjectName, subObject.`type`), "Invalid ref (%s)." format item.ref)
                    logger.debug("didn't know what to do with " + item.ref)
                  }
                }
              }
              case _ =>
            }
          }
          else if (subObject.items != null && subObject.items != None && subObject.items.get.`type` != null) {
            subObject.items foreach { item =>
              (getUpdatedType(validModelNames, item.`type`) map { updatedType =>
                if (!item.`type`.equals(updatedType)) {
                  addError(model, MODEL_PROPERTY, "%s->%s: %s" format (model.id, subObjectName, subObject.`type`), "Invalid type (%s). Best guess: %s" format (item.`type`, updatedType))
                  logger.debug("updated subObject.items.type" + item.`type` + " to " + updatedType)
                  if (fix) subObject.items = Some(ModelRef(`type` = updatedType))
                }
              }) getOrElse {
                println("nothing found for " + subObject)
                addError(model, MODEL_PROPERTY, "%s->%s: %s" format (model.id, subObjectName, subObject.`type`), "Invalid ref (%s)." format item.ref)
                logger.debug("didn't know what to do with " + item.ref)
              }
            }
          }
        } else {
          getUpdatedType(validModelNames, subObject.`type`) foreach { updatedType =>
            if (!subObject.`type`.equals(updatedType)) {
              addError(model, MODEL_PROPERTY, "%s->%s: %s" format (model.id, subObjectName, subObject.`type`), "Invalid type (%s). Best guess: %s" format (subObject.`type`, updatedType))
              logger.debug("updated subObject.getType " + subObject.`type` + " to " + updatedType)
              if (fix) subObject.`type` = updatedType
            }
          }
        }
      })
      // remove params with invalid names (Pos???)
      model.properties = model.properties.filter(prop => {
        if (prop._1.indexOf("$") == -1) true
        else {
          addError(model, MODEL, model.id, "Invalid property %s. Removing it" format prop._1)
          logger.debug("removing invalid property " + prop._1)
          if (fix) false else true
        }
      })
    }
  }

  /**
   * this is here because input params in operations don't match primitives or model names
   */
  private def fixInputDataTypes(models: Map[String, Model], a: List[ApiListing]) {
    val validModelNames = models.map(m => m._1).toSet

    // List[ApiListing]
    a.foreach(listing => {
      if (listing.apis != null) {
        listing.apis.foreach(api => {
          // List[ApiDescription]
          api.operations.foreach(op => {
            // List[Operation]
            if(op.parameters != null) {
              op.parameters.foreach(p => {
                val dataType = p.dataType

                p.paramType match {
                  case "body" => {
                    getUpdatedType(validModelNames, dataType) foreach { updatedName =>
                      if (!p.dataType.equals(updatedName)) {
                        //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                        addError(p, OPERATION_PARAM, "%s.%s(body: %s)" format (apiNameFromPath(api.path), op.nickname, p.dataType), "Invalid data type %s. Best guess: %s" format (p.dataType, updatedName))
                        if (fix) p.dataType = updatedName
                      }
                    }
                  }
                  case "path" => {
                    getUpdatedType(validModelNames, dataType) foreach { updatedName =>
                      addError(p, OPERATION_PARAM, "%s.%s(path_%s: %s)" format (apiNameFromPath(api.path), op.nickname, p.name, p.dataType), "Invalid data type %s. Best guess: %s" format (p.dataType, updatedName))
                      if (fix) p.dataType = updatedName
                    }
                  }
                  case "query" => {
                    getUpdatedType(validModelNames, dataType) foreach { updatedName =>
                      addError(p, OPERATION_PARAM, "%s.%s(query_%s: %s)" format (apiNameFromPath(api.path), op.nickname, p.name, p.dataType), "Invalid %s. Best guess: %s" format (p.dataType, updatedName))
                      if (fix) p.dataType = updatedName
                    }
                  }
                  case _ =>
                }

              })
            }
          })
        })
      }
    })
  }

  /**
   * this is here because the return types are inconsistent from the swagger-core-1.02-SNAPSHOT
   */
  private def fixReturnModels(models: Map[String, Model], a: List[ApiListing]) {
    val validModelNames = models.map(m => m._1).toSet

    // List[ApiListing]
    a.foreach(listing => {
      if (listing.apis != null) {
        listing.apis.foreach(api => {
          // List[ApiDescription]
          api.operations.foreach(op => {
            // List[Operation]
            val responseClass = op.responseClass
            if (responseClass != null) {
              getUpdatedType(validModelNames, responseClass) foreach { updatedName =>
                if (!responseClass.equals(updatedName)) {
                  logger.debug("--> updated " + responseClass + " to " + updatedName)
                  addError(op, OPERATION, "%s.%s(): %s" format (apiNameFromPath(api.path), op.nickname, op.responseClass), "Invalid response class. Best guess: %s" format updatedName)
                  if (fix) op.responseClass = updatedName
                }
              }
            }
          })
        })
      }
    })
  }

  private def getUpdatedType(validModelNames: Set[String], name: String): Option[String] = {
    val ComplexTypeMatcher = ".*\\[(.*)\\].*".r

    name match {
      case v if validModelNames.contains(v) => Some(v)
      case v @ ComplexTypeMatcher(basePart) if v.indexOf("[") > 0 =>
        getUpdatedType(validModelNames, basePart) map (name.replaceAll(java.util.regex.Pattern.quote(basePart), _))
      case v if (name.indexOf(".") > 0) =>
        val basePart = name.split("\\.").last
        getUpdatedType(validModelNames, basePart)
      case "Ok" if !primitives.contains(name) => Some("void")
      case "Long" if !primitives.contains(name) => Some("long")
      case "Double" if !primitives.contains(name) => Some("double")
      case "Float" if !primitives.contains(name) => Some("float")
      case "Boolean" if !primitives.contains(name) => Some("boolean")
      case "Integer" if !primitives.contains(name) => Some("int")
      case "Byte" if !primitives.contains(name) => Some("byte")
      case _ => None
    }
  }

  def addError(element: AnyRef, elementType: String, elementId: String, message: String, level: String = ERROR) {
    validationMessages += new ValidationMessage(element, elementType, elementId, message, level)
  }

  override def toString = {
    val out = new StringBuilder
    for (v <- validationMessages) {
      out.append(v)
      out.append('\n')
    }

    out.toString()
  }
}

class ValidationMessage(val element: AnyRef, val elementType: String, val elementId: String, val message: String, val level: String) {
  override def toString = level + ": " + elementType + " - " + elementId + " | " + message
}

object ValidationMessage {
  val WARNING = "Warning"
  val ERROR = "Error"

  val RESOURCE_LISTING = "Root Resources Listing"
  val RESOURCE = "Resource"
  val OPERATION = "Operation"
  val OPERATION_PARAM = "Operation Parameter"
  val MODEL = "Model"
  val MODEL_PROPERTY = "Model Property"
}
