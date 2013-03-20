package com.wordnik.swagger
package codegen

import model._
import util.{CoreUtils, ApiExtractor, ResourceExtractor}
import scala.util.control.Exception._
import java.io.File
import collection.mutable
import spec.SwaggerSpec._
import org.fusesource.scalate.{Binding, TemplateSource, Template, TemplateEngine}
import spec.SwaggerSpec
import org.apache.commons.io.FileUtils
import mojolly.inflector.InflectorImports._

class FetchResourceListing extends Phase[CompilerConfig, (GeneratorConfig, ResourceListing)] {
  def apply(context: CompilerConfig): (GeneratorConfig, ResourceListing) = {
    val host = context.host
    val apiKey = context.apiKey
    val format = context.formatter
    allCatch.withApply(e => throw new Exception("Unable to read from " + host, e)) {
      val doc = ResourceExtractor.fetchListing(format.resourcePath(host), apiKey)
      (GeneratorConfig(context, format.basePath(doc.basePath)), doc)
    }
  }
}

class FetchApiListing extends Phase[(GeneratorConfig, ResourceListing), (GeneratorConfig, ResourceListing, List[ApiListing])] {
  def apply(context: (GeneratorConfig, ResourceListing)): (GeneratorConfig, ResourceListing, List[ApiListing]) = {
    val (gen, list) = context
    val apiKey = gen.compilerConfig.apiKey
    require(list.apis != null && list.apis.nonEmpty, "No API's specified by resource")
    (gen, list, ApiExtractor.fetchApiListings(gen.basePath, list.apis, apiKey))
  }
}

object FetchListings { def apply(): FetchListings = new FetchListings }
class FetchListings extends Phase[CompilerConfig, (GeneratorConfig, ResourceListing, List[ApiListing])] {
  def apply(context: CompilerConfig): (GeneratorConfig, ResourceListing, List[ApiListing]) = {
    (new FetchResourceListing andThen new FetchApiListing)(context)
  }
}

case class GeneratorContext(
  allModels: Map[String, Model],
  operationMap: Map[(String, String), List[(String, Operation)]],
  modelBundle: List[ModelBundleItem],
  apiBundle: List[ApiBundleItem],
  config: GeneratorConfig) {
  def compilerConfig = config.compilerConfig
  def format = compilerConfig.formatter
  def languageConfig = format.languageConfig
}
object CreateBundles { def apply(): CreateBundles = new CreateBundles }
class CreateBundles extends Phase[(GeneratorConfig, ResourceListing, List[ApiListing]), GeneratorContext] {
  def apply(context: (GeneratorConfig, ResourceListing, List[ApiListing])): GeneratorContext = {
    val (config, _, apiListing) = context
    val format = config.compilerConfig.formatter
    val (operations, allModels) = apiOperationsAndModels(apiListing, format)
    val operationMap = groupOperationsToFiles(operations, format)
    val modelBundle = prepareModelBundle(allModels, config, format).toList
    val apiBundle = prepareApiBundle(operationMap, config, format).toList
    GeneratorContext(allModels, operationMap, modelBundle, apiBundle, config)
  }

  private[this] def apiOperationsAndModels(apiListing: List[ApiListing], format: Formatter): (List[(String, String, Operation)], Map[String, Model]) =
    apiListing.foldLeft((List.empty[(String, String, Operation)], Map.empty[String, Model])) {
      case ((operations, models), listing) =>
        val basePath = listing.basePath
        val apis = if (listing.apis != null && listing.apis.nonEmpty) listing.apis else Nil
        val extracted =
          for {
            api <- apis
            (apiPath, op) <- ApiExtractor.extractApiOperations(basePath, api)
            operation = format.processApiOperation(apiPath, op)
          } yield (basePath, apiPath, operation)

        (extracted ::: operations, models ++ CoreUtils.extractApiModels(listing))
    }

  private[this] def groupOperationsToFiles(operations: List[(String, String, Operation)], format: Formatter): Map[(String, String), List[(String, Operation)]] = {
    val entries =
      for {
        (basePath, apiPath, operation) <- operations
        className = format.resourceNameFromFullPath(apiPath)
      } yield ((basePath, className) -> List(apiPath -> operation))

    Map(entries:_*)
  }

  private[this] def prepareModelBundle(allModels: Map[String, Model], config: GeneratorConfig, format: Formatter) = {
    for {
      (name, schema) <- allModels
      if !format.languageConfig.defaultIncludes.contains(name)
    } yield ModelBundleItem(
      format.modelName(name),
      name,
      format.modelFilename(name),
      List(name -> schema),
      config.compilerConfig.destinationDir + File.separator + format.languageConfig.modelPackage.getOrElse("").replaceAll("\\.", File.separator)
    )
  }

  private[this] def prepareApiBundle(operationMap: Map[(String, String), List[(String, Operation)]], config: GeneratorConfig, format: Formatter) = {
    for {
      ((basePath, name), operationList) <- operationMap
      className = format.apiName(name)
    } yield ApiBundleItem(
      name,
      format.apiFilename(name),
      className,
      className,
      basePath,
      List(OperationBundleItem(className, operationList)),
      config.compilerConfig.destinationDir + File.separator + format.languageConfig.apiPackage.getOrElse("").replaceAll("\\.", File.separator)
    )
  }
}

trait TemplateContextBuildPhase[T] extends Phase[GeneratorContext, T] {
  protected def isListType(dt: String) = isCollectionType(dt, "List") || isCollectionType(dt, "Array")

  protected def isMapType(dt: String) = isCollectionType(dt, "Map")

  protected def isCollectionType(dt: String, str: String) = {
    dt == str || {
      val idx = dt.indexOf("[")
      idx > -1 && dt.substring(0, idx) == str
    }
  }
}

object BuildModelTemplateContext {
  object ModelContext {
    def empty = ModelContext(Set.empty, Set.empty, List.empty)
  }
  case class ModelContext(imports: Set[String], included: Set[String], list: List[ModelTemplateData])

  def apply(): BuildModelTemplateContext = new BuildModelTemplateContext
  def apply(context: GeneratorContext): ModelContext = (new BuildModelTemplateContext)(context)
}
class BuildModelTemplateContext extends TemplateContextBuildPhase[BuildModelTemplateContext.ModelContext] {
  import BuildModelTemplateContext._

  def apply(context: GeneratorContext): ModelContext = {
    val mc =
      context.modelBundle.foldLeft(ModelContext.empty) { (ctxt, bundleItem) =>
        val records = for {
          (className, bundleModel) <- bundleItem.models
          model = buildModelTemplateData(className, bundleModel, context.config.compilerConfig)
        } yield (model.imports, className, model)
        records.foldLeft(ctxt) {
          case (ModelContext(i, ic, mo), (ii, iicc, mmoo)) => ModelContext(i ++ ii, ic + iicc, mmoo :: mo)
        }
      }

    mc.copy(list = (mc.list.head.copy(hasMore = false) :: mc.list.tail).reverse)
  }

  protected def buildModelTemplateData(className: String, model: Model, config: CompilerConfig): ModelTemplateData = {
    val format = config.formatter

    val (imports, properties) = {
      model.properties.foldLeft((Set.empty[String], List.empty[ModelPropertyTemplateData])) {
        case ((imp, mods), (name, prop @ ModelProperty(dt, req, desc, allowed, items))) =>
          val propImports = mutable.HashSet.empty[String]
          val btInter = if (items != null) {
            propImports += dt
            items map (it => it.ref.getOrElse(it.`type`)) getOrElse dt
          } else dt
          val baseType = format.languageConfig.typeMapping.get(btInter) getOrElse {
            propImports += btInter
            btInter
          }
          val typeOverrides = (format.languageConfig.defaultIncludes ++ format.languageConfig.languageSpecificPrimitives).toSet
          if (!typeOverrides.contains(baseType)) {
            propImports += baseType
          }

          val isContainer = isListType(dt) || isMapType(dt)
          val isPrimitive = format.languageConfig.languageSpecificPrimitives.contains(baseType) || primitives.contains(baseType)

          val realBaseType =
            format.languageConfig.modelPackage.filter(_ => primitives.contains(baseType)).map(_+"."+baseType).getOrElse(baseType)
          val (datatype, default) = format.declaration(prop)

          val propData =
            ModelPropertyTemplateData(
              name = format.varName(name),
              nameSingular = format.varName(name).singularize,
              baseType = realBaseType,
              baseTypeVarName = format.varName(baseType),
              baseName = name,
              datatype = datatype,
              defaultValue = default,
              description = desc.orNull,
              notes = desc.orNull,
              required = req,
              getter = format.getter(name, datatype),
              setter = format.setter(name, datatype),
              isList = isListType(dt),
              isMap = isMapType(dt),
              isContainer = isContainer,
              isNotContainer = !isContainer,
              isPrimitive = isPrimitive,
              complexType = if (isPrimitive) null else format.modelName(baseType)
            )

          (imp ++ propImports.toSet, propData :: mods)
      }

    }

    format processModelMap {
      ModelTemplateData(
        classname = format.modelName(className),
        classVarName = format.varName(className),
        modelPackage = format.languageConfig.modelPackage,
        newLine = config.newLine,
        vars = (properties.head.copy(hasMore = false) :: properties.tail).reverse,
        imports = imports
      )
    }
  }
}

object BuildApiTemplateContext {
  object ApiContext {
    def empty = ApiContext(Set.empty, null, List.empty)
  }
  case class ApiContext(imports: Set[String], className: String, operations: List[OperationTemplateData])

  object OperationContext {
    def empty = OperationContext()
  }
  case class OperationContext(
         bodyParam: Option[String] = None,
         queryParams: List[ParameterTemplateData] = Nil,
         pathParams: List[ParameterTemplateData] = Nil,
         headerParams: List[ParameterTemplateData] = Nil,
         bodyParams: List[ParameterTemplateData] = Nil,
         paramsList: List[ParameterTemplateData] = Nil)

  def apply(context: GeneratorContext): List[ApiContext] = (new BuildApiTemplateContext)(context)
}
class BuildApiTemplateContext extends TemplateContextBuildPhase[List[BuildApiTemplateContext.ApiContext]] {

  import BuildApiTemplateContext._
  def apply(context: GeneratorContext): List[ApiContext] = {
    (context.apiBundle.foldLeft(List.empty[ApiContext]) { (ctxt, bundleItem) =>
      val apis = bundleItem.apis map { opsItem =>
        val item = for {
           (apiPath, op) <- opsItem.operations
          operation = buildOperationTemplateData(apiPath, op, context.compilerConfig)
          imports = CoreUtils.extractModelNames(op)
        } yield (operation, imports)
        item.foldLeft(ApiContext.empty) {
          case (acc, (operation, imports)) =>
            acc.copy(acc.imports ++ imports, opsItem.className, operation :: acc.operations)
        }
      }
      apis ::: ctxt
    })
  }

  protected def buildOperationTemplateData(path: String, operation: Operation, config: CompilerConfig): OperationTemplateData = {
    val format = config.formatter
    val opCtxt = operation.parameters.foldLeft(OperationContext.empty) {
      case (ctxt, param @ Parameter(name, desc, default, req, multi, dt, allowed, pt)) => {
        val pd = ParameterTemplateData(
          `type` = pt,
          defaultValue = format.defaultValue(dt, default),
          dataType = format.declaredType(dt),
          swaggerDataType = dt,
          description = desc,
          allowMultiple = multi,
          allowableValues = allowableValuesToString(allowed),
          optional = if (pt == "path") false else !req,
          required = if (pt == "path") true else req,
          paramName = if (pt == "body") "body" else format.varName(name),
          baseName = if (pt == "body") "body" else name,
          hasMore = true
        )
        ctxt.copy(
          bodyParam = if (pt == "body") Some("body") else None,
          queryParams = if (pt == "query") pd :: ctxt.queryParams else ctxt.queryParams,
          pathParams = if (pt == "path") pd :: ctxt.pathParams else ctxt.pathParams,
          headerParams = if (pt == "header") pd :: ctxt.headerParams else ctxt.headerParams,
          bodyParams = if (pt == "body") pd :: ctxt.bodyParams else ctxt.bodyParams,
          paramsList = pd :: ctxt.paramsList
        )
      }
    }

    def withHasMore(l: List[ParameterTemplateData]) = {
      val nq = if (l.isEmpty) l else (l.head.copy(hasMore = false) :: l.tail).reverse
      if (nq.isEmpty) nq else nq.head.copy(secondaryParam = false) :: nq.tail
    }

    val rp = for {
      param <- opCtxt.paramsList
      if param.required
    } yield RequiredParameterTemplateData(
        paramName = param.paramName,
        defaultValue = param.defaultValue,
        baseName = param.baseName
      )

    val requiredParams = if (rp.isEmpty) rp else (rp.head.copy(hasMore = false) :: rp.tail).reverse

    val operationContext = opCtxt.copy(
      queryParams = withHasMore(opCtxt.queryParams),
      pathParams = withHasMore(opCtxt.pathParams),
      headerParams = withHasMore(opCtxt.headerParams),
      bodyParams = withHasMore(opCtxt.bodyParams),
      paramsList = withHasMore(opCtxt.paramsList)
    )

    val (rt, rbt, rst, rtist) = {
      val prim = (format.languageConfig.languageSpecificPrimitives ++ primitives).toSet
      val idx = operation.responseClass.indexOf("[")
      if (idx > -1) {
        val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
        val ComplexTypeMatcher(bt) = operation.responseClass
        val t = operation.responseClass.replaceAll(bt, format.processResponseClass(bt).get)
        (format.processResponseDeclaration(t), format.processResponseClass(bt), Some(operation.responseClass.substring(0, idx)), prim.contains(bt))
      } else {
        val bt = operation.responseClass
        (format.processResponseDeclaration(bt), format.processResponseClass(bt), None, prim.contains(bt))
      }
    }


    val ot = OperationTemplateData(
      path = path,
      nickname = format.methodName(operation.nickname),
      summary = operation.summary,
      notes = operation.notes,
      deprecated = !operation.`deprecated`.forall(_ != "true"),
      bodyParam = operationContext.bodyParam,
      allParams = operationContext.paramsList,
      pathParams = operationContext.pathParams,
      queryParams = operationContext.queryParams,
      headerParams = operationContext.headerParams,
      bodyParams = operationContext.bodyParams,
      requiredParams = requiredParams,
      httpMethod = operation.httpMethod.toUpperCase,
      requiredParamCount = requiredParams.length,
      returnType = rt,
      returnBaseType = rbt,
      returnContainer = rst.getOrElse(""),
      returnSimpleType = rst.isEmpty,
      returnTypeIsPrimitive = rtist
    )
    format.processApiMap(ot)
  }
  def allowableValuesToString(v: AllowableValues) = {
    v match {
      case av: AllowableListValues => {
        Some(av.values.mkString("LIST[", ",", "]"))
      }
      case av: AllowableRangeValues => {
        Some("RANGE[" + av.min + "," + av.max + "]")
      }
      case _ => None
    }
  }
}

object BuildTemplateContext { def apply(): BuildTemplateContext = new BuildTemplateContext }

class BuildTemplateContext extends Phase[GeneratorContext, (GeneratorContext, TemplateContext)] {

  def apply(context: GeneratorContext): (GeneratorContext, TemplateContext) = {
    val modelContext = BuildModelTemplateContext(context)
    val apiContexts = BuildApiTemplateContext(context)
    val importScope = context.languageConfig.modelPackage map (_+".") getOrElse ""

    val (allImports, imports) = collectImports(modelContext, apiContexts, context, importScope)
    val requiredModels = collectRequiredModels(allImports)

    val modelTemplateContexts = context.modelBundle map { item =>
      (item, TemplateData(
        name = item.name,
        `package` = context.languageConfig.modelPackage,
        baseName = null,
        className = item.className,
        invokerPackage = context.languageConfig.invokerPackage,
        imports = imports.toList,
        requiredModels = requiredModels,
        operations = Nil,
        models = ApiModelTemplateData(modelContext.list),
        basePath = ""
      ))
    }

    val apiTemplateContexts = context.apiBundle map { container =>
      (container, TemplateData(
        name = container.name,
        `package` = context.languageConfig.apiPackage,
        baseName = container.baseName,
        className = container.className,
        invokerPackage = context.languageConfig.invokerPackage,
        imports = imports.toList,
        requiredModels = requiredModels,
        operations = apiContexts.map(ac => ApiTemplateData(ac.className, ac.operations)),
        models = null,
        basePath = container.basepath
      ))
    }

    val supportingFileModels = for {
      (bundle, item) <- modelTemplateContexts
      tdata <- item.models.model
      (_, model) <- bundle.models
      modelJson = org.json4s.jackson.Serialization.write(model)(SwaggerSerializers.formats)
    } yield SupportingFileModelData(bundle.name, tdata, bundle.filename, modelJson, hasMore = true)

    val supportingFileApis = for {
      (bundle, _) <- apiTemplateContexts
      apiBundle <- bundle.apis
      operations = for { (path, op) <- apiBundle.operations } yield SupportingFileOperationData(op, path)
    } yield SupportingFileApiData(bundle.name, bundle.filename, bundle.className, bundle.basepath, operations)

    val supportingFiles = SupportingFileTemplateData(
      invokerPackage = context.languageConfig.invokerPackage,
      `package` = context.compilerConfig.packageName,
      modelPackage = context.languageConfig.modelPackage,
      apiPackage = context.languageConfig.apiPackage,
      models = supportingFileModels,
      apis = supportingFileApis
    )
    (context, TemplateContext(modelTemplateContexts, apiTemplateContexts, supportingFiles))
  }

  def collectRequiredModels(allImports: Set[String]): List[RequiredModel] = {
    val allReqs = allImports.foldRight(List.empty[RequiredModel])(RequiredModel(_, hasMore = true) :: _)
    if (allReqs.isEmpty) Nil else (allReqs.head.copy(hasMore = false) :: allReqs.tail).reverse
  }

  def collectImports(modelContext: BuildModelTemplateContext.ModelContext, apiContexts: List[BuildApiTemplateContext.ApiContext], context: GeneratorContext, importScope: String) = {
    val apiImports = if (apiContexts.nonEmpty) apiContexts.map(_.imports).reduce(_ ++ _) else Set.empty[String]
    val allImports = modelContext.imports ++ apiImports
    val mappedImports = for {
      i <- allImports
      model = context.format.modelName(i)
      if (!modelContext.included.contains(model) && context.languageConfig.importMapping.contains(model))
    } yield ImportsData(context.languageConfig.importMapping(model))

    val trimmedImports = allImports -- context.languageConfig.defaultIncludes -- SwaggerSpec.excludes

    val remainingImports = for {
      i <- trimmedImports
      model = context.format.modelName(i)
      if (!modelContext.included.contains(model) && !context.languageConfig.importMapping.contains(model) && !mappedImports.contains(ImportsData(importScope + model)))
    } yield ImportsData(importScope + model)
    (allImports, mappedImports ++ remainingImports)
  }
}

object GenerateFiles {

  val ContextKey = "swaggerContext"
  def apply():GenerateFiles = new GenerateFiles
}
class GenerateFiles extends Phase[(GeneratorContext, TemplateContext), List[File]] {

  import GenerateFiles._

  def apply(ctx: (GeneratorContext, TemplateContext)): List[File] = {
    val (context, templateContext) = ctx
    generateModels(context, templateContext.models) :::
      generateApis(context, templateContext.apis) :::
      writeSupportingFiles(context, templateContext.supportingFiles)
  }

  import sys.process._
  protected def generateModels(context: GeneratorContext, models: List[(ModelBundleItem, TemplateData)]): List[File] = {
    val engine = engineFor[TemplateData](context)
    try {
      for {
        (item, model) <- models
        (file, ext) <- context.compilerConfig.modelTemplateFiles
        out = new File(item.outDir, item.filename + ext)
      } yield compile(engine, file, out, model)
    } finally {
      engine.shutdown()
    }
  }

  protected def generateApis(context: GeneratorContext, apis: List[(ApiBundleItem, TemplateData)]): List[File] = {
    val engine = engineFor[TemplateData](context)
    try {
      for {
        (item, api) <- apis
        (file, ext) <- context.compilerConfig.apiTemplateFiles
        out = new File(item.outDir, item.filename + ext)
      } yield compile(engine, file, out, api)
    } finally {
      engine.shutdown()
    }
  }


  protected def writeSupportingFiles(context: GeneratorContext, templateContext: SupportingFileTemplateData): List[File] = {
    val engine = engineFor[SupportingFileTemplateData](context)
    try {
      for {
        (file, outputDir, destination) <- context.languageConfig.supportingFiles
        out = new File(outputDir, destination)
      } yield compileOrCopy(engine, file, out, templateContext)
    } finally {
      engine.shutdown()
    }
  }

  private[this] def compile[T](engine: TemplateEngine, template: String, out: File, data: T): File = {
    if (out.getParentFile != null && !out.getParentFile.exists()) out.getParentFile.mkdirs()
    val exitCode = (engine.layout(engine.source(template), Map(ContextKey -> data)) #> out !)
    require(exitCode > 0, "Failed to write template " + out)
    out
  }

  private[this] def compileOrCopy[T](engine: TemplateEngine, template: String, out: File, data: T): File = {
    if (template.endsWith(".mustache")) compile(engine, template, out, data)
    else {
      require(out.getParentFile != null, "You need to specify a directory to copy stuff into")
      val sourceFile = new File(engine.sourceDirectories.head, template)
      if (sourceFile.isDirectory) {
        FileUtils.copyDirectory(sourceFile, out.getParentFile)
        out.getParentFile
      } else {
        if (out.getParentFile != null && !out.getParentFile.exists()) out.getParentFile.mkdirs()
        FileUtils.copyFile(sourceFile, out, true)
        out
      }
    }
  }

  private[this] def engineFor[T](context: GeneratorContext)(implicit mf: Manifest[T]) = {
    val engine = new TemplateEngine(Seq(templateDir(context)))
    engine.escapeMarkup = false
    engine.bindings ::= Binding(ContextKey, mf.erasure.getName, importMembers = true)
    engine
  }

  private[this] def templateDir(context: GeneratorContext) = new File(context.compilerConfig.templateDir)



}