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
    apiPackage = "io.swagger.client.api";
    modelPackage = "io.swagger.client.model";

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

  /**
   * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
   * those terms here.  This logic is only called if a variable matches the reseved words
   * 
   * @return the escaped term
   */
  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;  // add an underscore to the name
  }

  /**
   * Location to write model files.  You can use the modelPackage() as defined when the class is
   * instantiated
   */
  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  /**
   * Location to write api files.  You can use the apiPackage() as defined when the class is
   * instantiated
   */
  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  /**
   * Optional - type declaration.  This is a String which is used by the templates to instantiate your
   * types.  There is typically special handling for different property types
   *
   * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
   */
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

  /**
   * Optional - swagger type conversion.  This is used to map swagger types in a `Property` into 
   * either language specific types via `typeMapping` or into complex models if there is not a mapping.
   *
   * @return a string value of the type or complex model for this property
   * @see com.wordnik.swagger.models.properties.Property
   */
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