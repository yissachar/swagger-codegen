package com.wordnik.swagger.codegen
package config

case class CompilerConfig(
    packageName: String,
    templateDir: String,
    destinationDir: String,
    apiTemplateFiles: Map[String, String] = Map.empty,
    modelTemplateFiles: Map[String, String] = Map.empty,
    apisToProcess: Set[String] = Set.empty,
    modelsToProcess: Set[String] = Set.empty,
    newLine: String = sys.props("line.separator"),
    formatter: format.Formatter,
    host: String = "http://localhost:8080/api-docs",
    apiKey: Option[String] = None)

case class GeneratorConfig(compilerConfig: CompilerConfig, basePath: String)