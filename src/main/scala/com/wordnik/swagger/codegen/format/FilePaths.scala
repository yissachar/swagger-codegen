package com.wordnik.swagger.codegen.format

import mojolly.inflector.InflectorImports._

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