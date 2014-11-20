package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.CodegenConfig;
import com.wordnik.swagger.codegen.DefaultCodegen;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.HashSet;

public class GolangClientCodegen extends DefaultCodegen implements CodegenConfig{

    public GolangClientCodegen() {
        super();
        outputFolder = "generated-code/golang";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");
        templateDir = "golang";
        apiPackage = "api";
        modelPackage = "model";

        defaultIncludes = new HashSet<String>(Arrays.asList(
            "bool", "int", "int32", "int64", "int8", "int16",
            "uint8", "uint16", "uint32", "uint64", "float32",
            "float64", "complex64", "complex128", "uint", "rune", "byte", "string"
        ));
        reservedWords = new HashSet<String>(
          Arrays.asList(
            "break", "default", "func", "interface", "select", "case", "defer",
            "go", "map", "struct", "chan", "else", "goto", "package", "switch",
            "const", "fallthrough", "if", "range", "type", "continue", "for", "import",
            "return", "var", "bool", "int", "int32", "int64", "int8", "int16", "uint8",
            "uint16", "uint32", "uint64", "float32", "float64", "complex64", "complex128",
            "uint", "rune", "byte", "string"));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replaceAll("\\.", "/");
    }

    public String modelFileFolder() {
        return outputFolder + "/" + modelPackage().replaceAll("\\.", "/");
    }

    @Override
    public String toOperationId(String operationId) {
        return initialCaps(operationId);
    }
}
