package com.wordnik.swagger.codegen.compiler

import com.wordnik.swagger.model.{Operation, Model}

case class CompilerConfig(
    packageName: String,
    templateDir: String,
    destinationDir: String,
    apiTemplateFiles: Map[String, String] = Map.empty,
    modelTemplateFiles: Map[String, String] = Map.empty,
    apisToProcess: Set[String] = Set.empty,
    modelsToProcess: Set[String] = Set.empty,
    newLine: String = sys.props("line.separator"),
    formatter: Formatter,
    host: String = "http://localhost:8080/api-docs",
    apiKey: Option[String] = None)

case class GeneratorConfig(compilerConfig: CompilerConfig, basePath: String)

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
case class ApiTemplateData(classname: String, operation: List[OperationTemplateData])
case class RequiredModel(name: String, hasMore: Boolean = false)
case class ImportsData(`import`: String) {
  override def toString: String = "import " + `import`
}

case class TemplateData (
  name: String,
  `package`: Option[String],
  baseName: String,
  className: String,
  invokerPackage: Option[String],
  imports: List[ImportsData],
  requiredModels: List[RequiredModel],
  operations: List[ApiTemplateData],
  models: ApiModelTemplateData,
  basePath: String
)
case class SupportingFileOperationData(operation: Operation, path: String)
case class SupportingFileApiData(name: String, filename: String, className: String, basePath: String, operations: List[SupportingFileOperationData])
case class SupportingFileModelData(modelName: String, model: ModelTemplateData, filename: String, modelJson: String, hasMore: Boolean = true)
case class SupportingFileTemplateData (
  invokerPackage: Option[String],
  `package`: String,
  modelPackage: Option[String],
  apiPackage: Option[String],
  apis: List[SupportingFileApiData],
  models: List[SupportingFileModelData]
)

case class TemplateContext(models: List[(ModelBundleItem, TemplateData)], apis: List[(ApiBundleItem, TemplateData)], supportingFiles: SupportingFileTemplateData)