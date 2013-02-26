package com.wordnik.swagger.codegen.compiler

import com.wordnik.swagger.model.{Operation, Model}

case class ApiBundleItem(
  baseName: String,
  filename: String,
  name: String,
  className: String,
  basepath: String,
  apis: List[OperationBundleItem],
  outDir: String)

case class OperationBundleItem(className: String, operations: List[(String, Operation)])

case class ModelBundleItem(
  name: String,
  className: String,
  filename: String,
  models: List[(String, Model)],
  outDir: String)