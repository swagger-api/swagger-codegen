package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class SwiftGenerator extends DefaultCodegen implements CodegenConfig {
  protected String sourceFolder = "Classes";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "swift";
  }

  public String getHelp() {
    return "Generates a swift client library.";
  }

  public SwiftGenerator() {
    super();
    outputFolder = "generated-code/swift";
    modelTemplateFiles.put("model.mustache", ".swift");
    apiTemplateFiles.put("api.mustache", ".swift");
    templateDir = "swift";
    apiPackage = "Apis";
    modelPackage = "Models";

    // from ObjcClientCodegen.java
    String appName = System.getProperty("appName");
    if(appName == null) {
      appName = "swaggerClient";
    }
    additionalProperties.put("projectName", appName);
    sourceFolder = appName + "/" + sourceFolder;

    supportingFiles.add(new SupportingFile("Podfile.mustache", "", "Podfile"));

    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "Int",
        "Float",
        "Double",
        "Bool",
        "Void",
        "String",
        "Character")
    );
    defaultIncludes = new HashSet<String>(
      Arrays.asList(
        "NSDate",
        "Array",
        "Dictionary",
        "Set",
        "Any",
        "AnyObject")
    );
    reservedWords = new HashSet<String>(
      Arrays.asList(
        "class", "break", "as", "associativity", "deinit", "case", "dynamicType", "convenience", "enum", "continue",
        "false", "dynamic", "extension", "default", "is", "didSet", "func", "do", "nil", "final", "import", "else",
        "self", "get", "init", "fallthrough", "Self", "infix", "internal", "for", "super", "inout", "let", "if",
        "true", "lazy", "operator", "in", "COLUMN", "left", "private", "return", "FILE", "mutating", "protocol",
        "switch", "FUNCTION", "none", "public", "where", "LINE", "nonmutating", "static", "while", "optional",
        "struct", "override", "subscript", "postfix", "typealias", "precedence", "var", "prefix", "Protocol",
        "required", "right", "set", "Type", "unowned", "weak")
    );

    typeMapping = new HashMap<String, String>();
    typeMapping.put("array", "Array");
    typeMapping.put("List", "Array");
    typeMapping.put("map", "Dictionary");
    typeMapping.put("Date", "NSDate");
    typeMapping.put("DateTime", "NSDate");
    typeMapping.put("boolean", "Bool");
    typeMapping.put("string", "String");
    typeMapping.put("char", "Character");
    typeMapping.put("short", "Int");
    typeMapping.put("int", "Int");
    typeMapping.put("long", "Int");
    typeMapping.put("integer", "Int");
    typeMapping.put("float", "Float");
    typeMapping.put("number", "Double");
    typeMapping.put("double", "Double");
    typeMapping.put("object", "AnyObject");

    importMapping = new HashMap<String, String>();
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;  // add an underscore to the name
  }

  @Override
  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
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
      return "[String:" + getTypeDeclaration(inner) + "]";
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

  @Override
  public String toDefaultValue(Property p) {
    // nil
    return null;
  }

  @Override
  public String toInstantiationType(Property p) {
    if (p instanceof MapProperty) {
      MapProperty ap = (MapProperty) p;
      String inner = getSwaggerType(ap.getAdditionalProperties());
      return "[String:" + inner + "]";
    } else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return "[" + inner + "]";
    } else
      return null;
  }

  @Override
  public CodegenProperty fromProperty(String name, Property p) {
    CodegenProperty codegenProperty = super.fromProperty(name, p);
    if (codegenProperty.isEnum) {
      List<Map<String, String>> swiftEnums = new ArrayList<Map<String, String>>();
      List<String> values = (List<String>) codegenProperty.allowableValues.get("values");
      for (String value : values) {
        Map<String, String> map = new HashMap<String, String>();
        map.put("enum", StringUtils.capitalize(value));
        map.put("raw", value);
        swiftEnums.add(map);
      }
      codegenProperty.allowableValues.put("values", swiftEnums);
      codegenProperty.datatypeWithEnum =
              StringUtils.left(codegenProperty.datatypeWithEnum, codegenProperty.datatypeWithEnum.length() - "Enum".length());
    }
    return codegenProperty;
  }
}