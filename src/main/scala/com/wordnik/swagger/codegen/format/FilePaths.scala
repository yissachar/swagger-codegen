package com.wordnik.swagger.codegen.format

trait FilePaths {
  def resourcePath(host: String) = {
    sys.props get "fileMap" map (_ + "/resources.json") getOrElse host
  }

  def basePath(basePath: String) = {
    System.getProperty("fileMap") match {
      case s: String => s
      case _ => basePath
    }
  }

  def modelName(name: String) = {
    name(0).toUpper + name.substring(1)
  }

  def apiName(name: String) = {
    name.replaceAll("\\{","").replaceAll("\\}", "") match {
      case s: String if(s.length > 0) => s(0).toUpper + s.substring(1) + "Api"
      case _ => "Api"
    }
  }

  def nameFromPath(apiPath: String) = {
    apiPath.split("/").last.split("\\.").head.replaceAll("/", "")
  }

  def apiNameFromPath(apiPath: String) = apiName(nameFromPath(apiPath))

  def resourceNameFromFullPath(apiPath: String) = {
    apiPath.split("/").last.split("\\.").head.replaceAll("/", "")
  }

  def modelFilename(name: String) = name
  def apiFilename(name: String) = apiName(name)
}