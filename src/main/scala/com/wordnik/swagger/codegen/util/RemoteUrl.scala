package com.wordnik.swagger.codegen.util

import scala.io.Source

trait RemoteUrl {
	def urlToString(url: String): String = Source.fromURL(url).mkString
}