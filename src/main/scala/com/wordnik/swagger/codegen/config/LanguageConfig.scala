package com.wordnik.swagger.codegen.config


case class LanguageConfig(
  defaultIncludes: Set[String] = Set.empty,
  languageSpecificPrimitives: Set[String] = Set.empty,
  typeMapping: Map[String, String] = Map.empty,
  reservedWords: Set[String] = Set.empty,
  importMapping: Map[String, String] = Map.empty,
  invokerPackage: Option[String] = None,
  apiPackage: Option[String] = None,
  modelPackage: Option[String] = None,
  supportingFiles: List[(String, String, String)] = List.empty)
