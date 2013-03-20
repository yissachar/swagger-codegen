package com.wordnik.swagger
package codegen

import config.{GeneratorConfig, CompilerConfig}
import java.io.File
import model.{ApiListing, ResourceListing}

package object compiler {
  type Phase[F, S] = F => S
  type Compiler = CompilerConfig => Seq[File]
  type FileGenerator = GeneratorContext => Seq[File]
  type BundleContext = (GeneratorConfig, ResourceListing, List[ApiListing])


  val DefaultCompiler: Compiler =
    ApiCompiler(FetchListings(), CreateBundles(), BuildTemplateContext() andThen GenerateFiles())

  object ApiCompiler {
    def apply(
      fetchListings: Phase[CompilerConfig, BundleContext],
      createBundles: Phase[BundleContext, GeneratorContext],
      generateFiles: FileGenerator): Compiler = fetchListings andThen createBundles andThen generateFiles
  }
}