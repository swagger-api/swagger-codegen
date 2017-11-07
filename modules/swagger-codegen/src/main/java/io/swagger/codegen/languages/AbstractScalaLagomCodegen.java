package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.*;

import java.io.File;
import java.util.*;

public abstract class AbstractScalaLagomCodegen extends DefaultCodegen {

  protected String modelPropertyNaming = "camelCase";
  protected String sourceFolder = "src/main/scala";

  public AbstractScalaLagomCodegen() {
    super();

    languageSpecificPrimitives.addAll(Arrays.asList(
        "String",
        "boolean",
        "Boolean",
        "Double",
        "Int",
        "Long",
        "Float",
        "Object",
        "Any",
        "List",
        "Seq",
        "Map"));

    cliOptions
        .add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
    cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
    cliOptions
        .add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
  }

  @Override
  public void processOpts() {
    super.processOpts();

    if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
      this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
    }
  }

  public void setSourceFolder(String sourceFolder) {
    this.sourceFolder = sourceFolder;
  }

  public String getSourceFolder() {
    return sourceFolder;
  }

  @Override
  public String escapeReservedWord(String name) {
    if (this.reservedWordsMappings().containsKey(name)) {
      return this.reservedWordsMappings().get(name);
    }
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  @Override
  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage()
        .replace('.', File.separatorChar);
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
    } else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();

      return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if (typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if (languageSpecificPrimitives.contains(type)) {
        return toModelName(type);
      }
    } else {
      type = swaggerType;
    }
    return toModelName(type);
  }

  @Override
  public String toInstantiationType(Property p) {
    if (p instanceof MapProperty) {
      MapProperty ap = (MapProperty) p;
      String inner = getSwaggerType(ap.getAdditionalProperties());
      return instantiationTypes.get("map") + "[String, " + inner + "]";
    } else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return instantiationTypes.get("array") + "[" + inner + "]";
    } else {
      return null;
    }
  }

  @Override
  public String toDefaultValue(Property p) {
    if (p instanceof StringProperty) {
      return "null";
    } else if (p instanceof BooleanProperty) {
      return "null";
    } else if (p instanceof DateProperty) {
      return "null";
    } else if (p instanceof DateTimeProperty) {
      return "null";
    } else if (p instanceof DoubleProperty) {
      return "null";
    } else if (p instanceof FloatProperty) {
      return "null";
    } else if (p instanceof IntegerProperty) {
      return "null";
    } else if (p instanceof LongProperty) {
      return "null";
    } else if (p instanceof MapProperty) {
      MapProperty ap = (MapProperty) p;
      String inner = getSwaggerType(ap.getAdditionalProperties());
      return "new HashMap[String, " + inner + "]() ";
    } else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return "new ListBuffer[" + inner + "]() ";
    } else {
      return "null";
    }
  }

  @Override
  public Map<String, Object> postProcessModels(Map<String, Object> objs) {
    // remove model imports to avoid warnings for importing class in the same package in Scala
    List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
    final String prefix = modelPackage() + ".";
    Iterator<Map<String, String>> iterator = imports.iterator();
    while (iterator.hasNext()) {
      String _import = iterator.next().get("import");
      if (_import.startsWith(prefix)) {
        iterator.remove();
      }
    }
    return postProcessModelsEnum(objs);
  }

  @Override
  public String toModelFilename(String name) {
    // should be the same as the model name
    return toModelName(name);
  }

  @Override
  public String escapeUnsafeCharacters(String input) {
    return input.replace("*/", "*_/").replace("/*", "/_*");
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
    ArrayList<CodegenOperation> oplist = (ArrayList<CodegenOperation>) operations.get("operation");

    for (CodegenOperation codegenOperation : oplist) {
      String path = codegenOperation.path;
      codegenOperation.path = path.replaceAll("\\{", ":").replaceAll("}", "");
    }
    return objs;
  }
}