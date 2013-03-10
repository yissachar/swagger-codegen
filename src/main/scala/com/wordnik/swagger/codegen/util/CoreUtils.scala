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

import scala.collection.mutable.{ HashSet, ListBuffer, HashMap }
import scala.collection.JavaConversions._
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import scala.io.Source
import collection.mutable

object CoreUtils {
  def extractAllModels(apis: List[ApiListing]): Map[String, Model] = {
    def extractFromApi(api: ApiListing): Map[String, Model] = extractApiModels(api) ++ api.models
    apis map extractFromApi reduce (_ ++ _)
  }

  def extractModelNames(op: Operation): Set[String] = {
    // POST, PUT, DELETE body
    val dataTypes = op.parameters.filter(p => p.paramType == "body").map(_.dataType)
    (op.responseClass :: dataTypes).toSet map extractBasePartFromType
  }

  def extractBasePartFromType(datatype: String): String = {
    val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
    datatype match {
      case ComplexTypeMatcher(basePart) => basePart
      case _ => datatype
    }
  }

  def extractApiModels(sd: ApiListing): Map[String, Model] = {
    val collectedNames =
      (for {
        api <- sd.apis
        op <- api.operations
        mod <- op.responseClass :: op.parameters.filter(_.paramType == "body").map(_.dataType)
      } yield mod).toSet

    // extract all base model names, strip away Containers like List[] and primitives
    val baseNames = collectedNames filterNot primitives.contains map extractBasePartFromType

    // get complex models from base
    val requiredModels = sd.models filterKeys baseNames.contains

    val subNames =
      requiredModels.foldLeft(Set.empty[String]){ case (acc, (_, model)) => collectSubModels(model, sd.models, acc)}

    val subModels = sd.models filterKeys subNames.contains
    val allModels = requiredModels ++ subModels
    allModels.filterKeys(!primitives.contains(_))
  }

  def collectSubModels(model: Model, allModels: Map[String, Model], collected: Set[String]): Set[String] = {
    model.properties.foldLeft(collected){ case (acc, (_, subObj)) =>
      val propName = if (containers.contains(subObj.`type`)) {
        subObj.items map { st => st.ref getOrElse st.`type` }
      } else Some(subObj.`type`)

      propName.filterNot(collected.contains).filter(allModels.contains) map { prop =>
        collectSubModels(allModels(prop), allModels, acc ++ Set(prop))
      } getOrElse acc
    }
  }

}