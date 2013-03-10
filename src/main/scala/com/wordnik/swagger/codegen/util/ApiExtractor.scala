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

package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.model._

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.read
import scala.io._
import util.control.Exception.allCatch

object ApiExtractor extends RemoteUrl {
  implicit val formats = SwaggerSerializers.formats

  private[this] def safeNotFound(basePath: String, api: ApiListingReference)(t: Throwable): Option[ApiListing] = t match {
    case e:java.io.FileNotFoundException =>
      println("WARNING!  Unable to read API " + basePath + api.path)
      None
    case _ => None
  }

  private[this] def readJson(basePath: String, api: ApiListingReference, apiKey: Option[String] = None): String = {
    if (basePath.startsWith("http")){
      println("calling: " + ((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")))
      urlToString((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json"))
    } else Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
  }

  def fetchApiListings(basePath: String, apis: List[ApiListingReference], apiKey: Option[String] = None): List[ApiListing] = {
    (for {
      api <- apis
      err = safeNotFound(basePath, api)(_)
      json = readJson(basePath, api, apiKey)
    } yield allCatch.withApply(err){ parseOpt(json) flatMap (_.extractOpt[ApiListing]) }).flatten
  }

  def extractApiOperations(basePath: String, references: List[ApiListingReference], apiKey: Option[String] = None) = {
    for {
      api <- references
      json = readJson(basePath, api, apiKey)
    } yield read[ApiListing](json)
  }

  def extractApiOperations(basePath: String, apiDescription: ApiDescription): List[(String, Operation)] =
    for(op <- apiDescription.operations) yield apiDescription.path -> op
}