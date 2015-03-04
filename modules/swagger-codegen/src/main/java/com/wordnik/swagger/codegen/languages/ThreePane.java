package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.Json;

import java.util.*;
import java.io.File;

public class ThreePane extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";
  protected String sourceFolder = "src/main/scala";

  public CodegenType getTag() {
    return CodegenType.DOCUMENTATION;
  }

  public String getName() {
    return "threepane";
  }

  public String getHelp() {
    return "Generates a static HTML file.";
  }

  public ThreePane() {
    super();
    outputFolder = "threepane";
    templateDir = "threepane";

    defaultIncludes = new HashSet<String>();

    List<String> codeSamples = new ArrayList<String>();
    codeSamples.add("curl");

    additionalProperties.put("code-samples", codeSamples);
    additionalProperties.put("appName", "Swagger Sample");
    additionalProperties.put("appDescription", "A sample swagger server");
    additionalProperties.put("infoUrl", "https://helloreverb.com");
    additionalProperties.put("infoEmail", "hello@helloreverb.com");
    additionalProperties.put("licenseInfo", "All rights reserved");
    additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    // languages
    supportingFiles.add(new SupportingFile("code_samples/curl.mustache", "code_samples", "curl.js"));

    // Main template
    supportingFiles.add(new SupportingFile("index.mustache", "", "index.html"));
    // Supporting files
    supportingFiles.add(new SupportingFile("assets/app.css", "", "app.css"));
    supportingFiles.add(new SupportingFile("assets/highlight.css", "", "highlight.css"));
    supportingFiles.add(new SupportingFile("assets/highlight.min.js", "assets", "highlight.min.js"));
    supportingFiles.add(new SupportingFile("assets/jquery.min.js", "assets", "jquery.min.js"));

    supportingFiles.add(new SupportingFile("images/logo_swagger.png", "images", "logo_swagger.png"));

    reservedWords = new HashSet<String>();

    languageSpecificPrimitives = new HashSet<String>();
    importMapping = new HashMap<String, String> ();
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();

      return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
    List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
    for(CodegenOperation op: operationList) {
      op.httpMethod = op.httpMethod.toLowerCase();
    }
    return objs;
  }
}
