package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class SwiftClientCodegen extends DefaultCodegen implements CodegenConfig {
  protected String sourceFolder = "client";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "swift";
  }

  public String getHelp() {
    return "Generates a Swift client library.";
  }

  public SwiftClientCodegen() {
    super();
    outputFolder = "generated-code/swift";
    modelTemplateFiles.put("model.mustache", ".swift");
    apiTemplateFiles.put("api.mustache", ".swift");
    templateDir = "swift";
    apiPackage = "api";
    modelPackage = "model";

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "__COLUMN__", "__FILE__", "__FUNCTION__", "__LINE__", "as", "associativity",
        "break", "case", "class", "continue", "convenience", "default", "deinit",
        "didSet", "do", "dynamic", "dynamicType", "else", "enum", "extension",
        "fallthrough", "false", "final", "for", "func", "get", "if", "import",
        "in", "infix", "init", "inout", "internal", "is", "lazy", "left", "let",
        "mutating", "nil", "none", "nonmutating", "operator", "optional", "override",
        "postfix", "precedence", "prefix", "private", "protocol", "Protocol",
        "public", "required", "return", "right", "self", "Self", "set", "static",
        "struct", "subscript", "super", "switch", "true", "Type", "typealias",
        "unowned", "var", "weak", "where", "while", "willSet")
    );

    supportingFiles.add(new SupportingFile("apiInvoker.mustache", sourceFolder, "ApiInvoker.swift"));
    supportingFiles.add(new SupportingFile("JsonModel.swift", sourceFolder, "JsonModel.swift"));
    supportingFiles.add(new SupportingFile("ApiCallback.swift", sourceFolder, "ApiCallback.swift"));
    supportingFiles.add(new SupportingFile("SWGDate.swift", sourceFolder, "SWGDate.swift"));
    supportingFiles.add(new SupportingFile("SWGFile.swift", sourceFolder, "SWGFile.swift"));
    supportingFiles.add(new SupportingFile("VoidResult.swift", sourceFolder, "VoidResult.swift"));
    supportingFiles.add(new SupportingFile("ApiException.swift", sourceFolder, "ApiException.swift"));

    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "String",
        "Bool",
        "Double",
        "Int",
        "UInt",
        "Float",
        "AnyObject")
      );

    typeMapping = new HashMap<String, String>();
    typeMapping.put("enum", "enum");
    typeMapping.put("Date", "SWGDate");
    typeMapping.put("DateTime", "SWGDate");
    typeMapping.put("boolean", "Bool");
    typeMapping.put("string", "String");
    typeMapping.put("integer", "Int");
    typeMapping.put("int", "Int");
    typeMapping.put("float", "Float");
    typeMapping.put("long", "Int");
    typeMapping.put("double", "Double");
    typeMapping.put("array", "Array");
    typeMapping.put("map", "Dictionary");
    typeMapping.put("number", "Double");
    typeMapping.put("List", "Array");
    typeMapping.put("object", "AnyObject");
    typeMapping.put("File", "SWGFile");

    instantiationTypes.put("array", "Array");
    instantiationTypes.put("map", "Dictionary");
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return "[" + getTypeDeclaration(inner) + "]";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();

      return"[String: " + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type))
        return toModelName(type);
    }
    else
      type = swaggerType;
    return toModelName(type);
  }
}