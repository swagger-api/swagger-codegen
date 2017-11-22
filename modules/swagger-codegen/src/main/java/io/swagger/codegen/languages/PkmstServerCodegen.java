package io.swagger.codegen.languages;


import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Path;
import io.swagger.models.Swagger;
import io.swagger.models.Tag;
import io.swagger.models.parameters.FormParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;

/**
 * Created by prokarma on 04/09/17.
 */
public class PkmstServerCodegen extends DefaultCodegen implements CodegenConfig {

  public static final String FULL_JAVA_UTIL = "fullJavaUtil";
  public static final String SUPPORT_JAVA6 = "supportJava6";
  public static final String CONFIG_PACKAGE = "configPackage";
  public static final String BASE_PACKAGE = "basePackage";
  public static final String TITLE = "title";
  public static final String WITH_XML = "withXml";
  public static final String EUREKA_URI = "eurekaUri";
  public static final String ZIPKIN_URI = "zipkinUri";
  public static final String SPRINGADMIN_URI = "springBootAdminUri";
  protected String groupId = "com.prokarma";
  protected String artifactId = "pkmst-microservice";
  protected String artifactVersion = "1.0.0";
  protected String projectFolder;
  

protected String projectTestFolder;
  protected String sourceFolder;
  protected String testFolder;
  protected String basePackage = "com.prokarma.pkmst";
  protected String serviceName = "Pkmst";
  protected String configPackage = "com.prokarma.pkmst.config";
  protected boolean implicitHeaders = false;
  protected boolean serializeBigDecimalAsString = false;
  protected boolean withXml = false;
  protected boolean fullJavaUtil;
  protected String javaUtilPrefix = "";
  protected Boolean serializableModel = false;
  protected String invokerPackage;
  protected String title;
  protected String apiDocPath = "docs/";
  protected String modelDocPath = "docs/";
  protected String eurekaUri;
  protected String zipkinUri;
  protected String springBootAdminUri;

  public PkmstServerCodegen() {
	  super();
    this.projectFolder = "src" + File.separator + "main";
    this.projectTestFolder = "src" + File.separator + "test";
    this.sourceFolder = this.projectFolder + File.separator + "java";
    this.testFolder = this.projectTestFolder + File.separator + "java";
    embeddedTemplateDir = templateDir = "pkmst";
    apiPackage = "com.prokarma.pkmst.controller";
	modelPackage = "com.prokarma.pkmst.model";
	invokerPackage = "com.prokarma.pkmst.controller";
    setReservedWordsLowerCase(
        Arrays.asList(
            // used as internal variables, can collide with parameter names
            "localVarPath", "localVarQueryParams", "localVarCollectionQueryParams",
            "localVarHeaderParams", "localVarFormParams", "localVarPostBody",
            "localVarAccepts", "localVarAccept", "localVarContentTypes",
            "localVarContentType", "localVarAuthNames", "localReturnType",
            "ApiClient", "ApiException", "ApiResponse", "Configuration", "StringUtil",

            // language reserved words
            "abstract", "continue", "for", "new", "switch", "assert",
            "default", "if", "package", "synchronized", "boolean", "do", "goto", "private",
            "this", "break", "double", "implements", "protected", "throw", "byte", "else",
            "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
            "catch", "extends", "int", "short", "try", "char", "final", "interface", "static",
            "void", "class", "finally", "long", "strictfp", "volatile", "const", "float",
            "native", "super", "while", "null")
    );

    this.languageSpecificPrimitives = new HashSet<String>(
        Arrays.asList(
            "String",
            "boolean",
            "Boolean",
            "Double",
            "Integer",
            "Long",
            "Float",
            "Object",
            "byte[]")
    );
    this.instantiationTypes.put("array", "ArrayList");
    this.instantiationTypes.put("map", "HashMap");
    this.typeMapping.put("date", "Date");
    this.typeMapping.put("file", "File");

    this.cliOptions.add(new CliOption("groupId", "groupId in generated pom.xml"));
    this.cliOptions.add(new CliOption("artifactId", "artifactId in generated pom.xml"));
    this.cliOptions.add(new CliOption("artifactVersion", "artifact version in generated pom.xml"));
    this.cliOptions.add(new CliOption("basePackage", "base package for java source code"));
    this.cliOptions.add(new CliOption("serviceName", "Service Name"));
    this.cliOptions.add(new CliOption(TITLE, "server title name or client service name"));
    this.cliOptions.add(new CliOption("eurekaUri", "Eureka URI"));
    this.cliOptions.add(new CliOption("zipkinUri", "Zipkin URI"));
    this.cliOptions.add(new CliOption("springBootAdminUri", "Spring-Boot URI"));
  //Middleware config
    this.cliOptions.add(new CliOption("pkmstInterceptor", "PKMST Interceptor"));
    this.apiTestTemplateFiles.put("api_test.mustache", ".java");
    this.modelDocTemplateFiles.put("model_doc.mustache", ".md");
    this.apiDocTemplateFiles.put("api_doc.mustache", ".md");
  }

  private static CodegenModel reconcileInlineEnums(CodegenModel codegenModel,
      CodegenModel parentCodegenModel) {
    // This generator uses inline classes to define enums, which breaks when
    // dealing with models that have subTypes. To clean this up, we will analyze
    // the parent and child models, look for enums that match, and remove
    // them from the child models and leave them in the parent.
    // Because the child models extend the parents, the enums will be available via the parent.

    // Only bother with reconciliation if the parent model has enums.
    if (!parentCodegenModel.hasEnums) {
      return codegenModel;
    }

    // Get the properties for the parent and child models
    final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
    List<CodegenProperty> codegenProperties = codegenModel.vars;

    // Iterate over all of the parent model properties
    boolean removedChildEnum = false;
    for (CodegenProperty parentModelCodegenPropery : parentModelCodegenProperties) {
      // Look for enums
      if (parentModelCodegenPropery.isEnum) {
        // Now that we have found an enum in the parent class,
        // and search the child class for the same enum.
        Iterator<CodegenProperty> iterator = codegenProperties.iterator();
        while (iterator.hasNext()) {
          CodegenProperty codegenProperty = iterator.next();
          if (codegenProperty.isEnum && codegenProperty.equals(parentModelCodegenPropery)) {
            // We found an enum in the child class that is
            // a duplicate of the one in the parent, so remove it.
            iterator.remove();
            removedChildEnum = true;
          }
        }
      }
    }

    if (removedChildEnum) {
      // If we removed an entry from this model's vars, we need to ensure hasMore is updated
      int count = 0, numVars = codegenProperties.size();
      for (CodegenProperty codegenProperty : codegenProperties) {
        count += 1;
        codegenProperty.hasMore = (count < numVars) ? true : false;
      }
      codegenModel.vars = codegenProperties;
    }
    return codegenModel;
  }

  private static String getAccept(Operation operation) {
    String accepts = null;
    String defaultContentType = "application/json";
    if (operation.getProduces() != null && !operation.getProduces().isEmpty()) {
      StringBuilder sb = new StringBuilder();
      for (String produces : operation.getProduces()) {
        if (defaultContentType.equalsIgnoreCase(produces)) {
          accepts = defaultContentType;
          break;
        } else {
          if (sb.length() > 0) {
            sb.append(",");
          }
          sb.append(produces);
        }
      }
      if (accepts == null) {
        accepts = sb.toString();
      }
    } else {
      accepts = defaultContentType;
    }

    return accepts;
  }

  public CodegenType getTag() {
    return CodegenType.SERVER;
  }

  public String getName() {
    return "pkmst";
  }

  public String getHelp() {
    return "Generates a Java SpringBoot Server application using the SpringFox integration."
        + " Also enables EurekaServerClient / Zipkin / Spring-Boot admin";
  }

  public void processOpts() {
    super.processOpts();
    if (this.additionalProperties.containsKey("basePackage")) {
      this.setBasePackage((String) this.additionalProperties.get("basePackage"));
      this.setInvokerPackage(this.getBasePackage());
      this.apiPackage = this.getBasePackage() + ".controller";
      this.modelPackage = this.getBasePackage() + ".model";
      this.setConfigPackage(this.getBasePackage() + ".config");
      
      //this.additionalProperties.put(BASE_PACKAGE, this.getBasePackage());
    }else{
    this.additionalProperties.put(BASE_PACKAGE, basePackage);
    this.additionalProperties.put(CONFIG_PACKAGE, this.getConfigPackage());
    this.additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
    this.additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
    this.additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
    }
    if (this.additionalProperties.containsKey("groupId")) {
      this.setGroupId((String) this.additionalProperties.get("groupId"));
    }else {
        //not set, use to be passed to template
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
    }
    if (this.additionalProperties.containsKey("artifactId")) {
      this.setArtifactId((String) this.additionalProperties.get("artifactId"));
    }else {
        //not set, use to be passed to template
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
    }
    if (this.additionalProperties.containsKey("artifactVersion")) {
      this.setArtifactVersion((String) this.additionalProperties.get("artifactVersion"));
    }else {
        //not set, use to be passed to template
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
    }
    if (this.additionalProperties.containsKey("serviceName")) {
      this.setServiceName((String) this.additionalProperties.get("serviceName"));
    }
    else {
        //not set, use to be passed to template
        additionalProperties.put("serviceName", serviceName);
    }

    if (this.additionalProperties.containsKey(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING)) {
      this.setSerializeBigDecimalAsString(Boolean.valueOf(
          this.additionalProperties.get(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING)
              .toString()));
    }
    if (this.additionalProperties.containsKey(CodegenConstants.SERIALIZABLE_MODEL)) {
      this.setSerializableModel(Boolean
          .valueOf(this.additionalProperties.get(CodegenConstants.SERIALIZABLE_MODEL).toString()));
    }
    if (this.additionalProperties.containsKey(TITLE)) {
      this.setTitle((String) this.additionalProperties.get(TITLE));
    }
    this.additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, serializableModel);
    if (this.additionalProperties.containsKey(FULL_JAVA_UTIL)) {
      this.setFullJavaUtil(
          Boolean.valueOf(this.additionalProperties.get(FULL_JAVA_UTIL).toString()));
    }
    
    if (this.additionalProperties.containsKey(EUREKA_URI)) {
        this.setEurekaUri((String) this.additionalProperties.get(EUREKA_URI));
      }
    if (this.additionalProperties.containsKey(ZIPKIN_URI)) {
        this.setZipkinUri((String) this.additionalProperties.get(ZIPKIN_URI));
      }
    if (this.additionalProperties.containsKey(SPRINGADMIN_URI)) {
        this.setSpringBootAdminUri((String) this.additionalProperties.get(SPRINGADMIN_URI));
      }
    if (fullJavaUtil) {
      javaUtilPrefix = "java.util.";
    }
    this.additionalProperties.put(FULL_JAVA_UTIL, fullJavaUtil);
    this.additionalProperties.put("javaUtilPrefix", javaUtilPrefix);
    this.additionalProperties.put(SUPPORT_JAVA6, false);
    this.additionalProperties.put("java8", true);

    if (this.additionalProperties.containsKey(WITH_XML)) {
      this.setWithXml(Boolean.valueOf(additionalProperties.get(WITH_XML).toString()));
    }
    this.additionalProperties.put(WITH_XML, withXml);

    // make api and model doc path available in mustache template
    this.additionalProperties.put("apiDocPath", apiDocPath);
    this.additionalProperties.put("modelDocPath", modelDocPath);

    this.importMapping.put("List", "java.util.List");

    if (fullJavaUtil) {
      this.typeMapping.put("array", "java.util.List");
      this.typeMapping.put("map", "java.util.Map");
      this.typeMapping.put("DateTime", "java.util.Date");
      this.typeMapping.put("UUID", "java.util.UUID");
      this.typeMapping.remove("List");
      this.importMapping.remove("Date");
      this.importMapping.remove("Map");
      this.importMapping.remove("HashMap");
      this.importMapping.remove("Array");
      this.importMapping.remove("ArrayList");
      this.importMapping.remove("List");
      this.importMapping.remove("Set");
      this.importMapping.remove("DateTime");
      this.importMapping.remove("UUID");
      this.instantiationTypes.put("array", "java.util.ArrayList");
      this.instantiationTypes.put("map", "java.util.HashMap");
    }
    // optional jackson mappings for BigDecimal support
    this.importMapping
        .put("ToStringSerializer", "com.fasterxml.jackson.databind.ser.std.ToStringSerializer");
    this.importMapping
        .put("JsonSerialize", "com.fasterxml.jackson.databind.annotation.JsonSerialize");

    // imports for pojos
    this.importMapping.put("ApiModelProperty", "io.swagger.annotations.ApiModelProperty");
    this.importMapping.put("ApiModel", "io.swagger.annotations.ApiModel");
    this.importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
    this.importMapping.put("JsonSubTypes", "com.fasterxml.jackson.annotation.JsonSubTypes");
    this.importMapping.put("JsonTypeInfo", "com.fasterxml.jackson.annotation.JsonTypeInfo");
    this.importMapping.put("JsonCreator", "com.fasterxml.jackson.annotation.JsonCreator");
    this.importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
    this.importMapping.put("SerializedName", "com.google.gson.annotations.SerializedName");
    this.importMapping.put("TypeAdapter", "com.google.gson.TypeAdapter");
    this.importMapping.put("JsonAdapter", "com.google.gson.annotations.JsonAdapter");
    this.importMapping.put("JsonReader", "com.google.gson.stream.JsonReader");
    this.importMapping.put("JsonWriter", "com.google.gson.stream.JsonWriter");
    this.importMapping.put("IOException", "java.io.IOException");
    this.importMapping.put("Objects", "java.util.Objects");
    this.importMapping.put("StringUtil", getInvokerPackage() + ".StringUtil");
    // import JsonCreator if JsonProperty is imported
    // used later in recursive import in postProcessingModels
    this.importMapping.put("com.fasterxml.jackson.annotation.JsonProperty",
        "com.fasterxml.jackson.annotation.JsonCreator");
    
    this.apiTemplateFiles.put("api.mustache", ".java");
    this.apiTemplateFiles.put("apiController.mustache", "Controller.java");

    this.modelTemplateFiles.put("model.mustache", ".java");

    this.supportingFiles.add(new SupportingFile(
        "SpringBootApplication.mustache",
        (this.getSourceFolder() + File.separator + this.getBasePackage())
            .replace(".", File.separator),
        this.getServiceName() + "Application" + ".java"));

    this.supportingFiles.add(new SupportingFile(
        "config" + File.separator + "swaggerDocumentationConfig.mustache",
        (this.sourceFolder + File.separator + this.getConfigPackage())
            .replace(".", java.io.File.separator) + File.separator + "swagger",
        "SwaggerDocumentationConfig.java"));

    this.supportingFiles.add(new SupportingFile(
        "config" + File.separator + "pkmstproperties.mustache",
        (this.sourceFolder + File.separator + this.getConfigPackage())
            .replace(".", java.io.File.separator) + File.separator + "swagger",
        "PkmstProperties.java"));
    this.supportingFiles.add(new SupportingFile(
            "config" + File.separator + "appconfig.mustache",
            (this.sourceFolder + File.separator + this.getConfigPackage())
                .replace(".", java.io.File.separator) + File.separator,
            "AppConfig.java"));

    // Security
    this.supportingFiles.add(new SupportingFile(
        "security" + File.separator + "authorizationServerConfiguration.mustache",
        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
            + File.separator + "security",
        "AuthorizationServerConfiguration.java"
    ));
    this.supportingFiles.add(new SupportingFile(
        "security" + File.separator + "oAuth2SecurityConfiguration.mustache",
        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
            + File.separator + "security",
        "OAuth2SecurityConfiguration.java"
    ));
    this.supportingFiles.add(new SupportingFile(
        "security" + File.separator + "resourceServerConfiguration.mustache",
        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
            + File.separator + "security",
        "ResourceServerConfiguration.java"
    ));

    // logging

    this.supportingFiles.add(new SupportingFile(
        "logging" + File.separator + "httpLoggingFilter.mustache",
        (this.sourceFolder + File.separator + this.basePackage).replace(".", File.separator)
            + File.separator + "logging",
        "HttpLoggingFilter.java"
    ));

    // Resources
    this.supportingFiles.add(new SupportingFile(
        "resources" + File.separator + "application-local.mustache",
        ("src.main.resources").replace(".", java.io.File.separator),
        "application-local.yml"));
    this.supportingFiles.add(new SupportingFile(
            "resources" + File.separator + "application-dev.mustache",
            ("src.main.resources").replace(".", java.io.File.separator),
            "application-dev.yml"));
    this.supportingFiles.add(new SupportingFile(
            "resources" + File.separator + "application-dev-config.mustache",
            ("src.main.resources").replace(".", java.io.File.separator),
            "application-dev-config.yml"));
    this.supportingFiles.add(new SupportingFile(
        "resources" + File.separator + "bootstrap.mustache",
        ("src.main.resources").replace(".", java.io.File.separator),
        "bootstrap.yml"));

    // POM
    this.supportingFiles.add(new SupportingFile(
        "pom.mustache",
        "",
        "pom.xml"));
    
    //Readme
    this.supportingFiles.add(new SupportingFile(
            "readme.mustache",
            "",
            "Readme.md"));
    
    //manifest
    
    this.supportingFiles.add(new SupportingFile(
            "manifest.mustache",
            "",
            "manifest.yml"));
    
    //docker
    this.supportingFiles.add(new SupportingFile(
            "docker.mustache",
            "",
            "Dockerfile"));
    
    //logstash
    
    this.supportingFiles.add(new SupportingFile(
            "logstash.mustache",
            "",
            "logstash.conf"));

    // Cucumber
    this.supportingFiles.add(new SupportingFile(
        "cucumber" + File.separator + "executeReport.mustache",
        this.testFolder + File.separator + this.basePackage .replace(".", File.separator)
        + File.separator +"cucumber" + File.separator+"report",
        "ExecuteReport.java"
    ));
    
    


        this.supportingFiles.add(new SupportingFile(
            "cucumber" + File.separator + "cucumberTest.mustache",
            this.testFolder + File.separator + this.basePackage .replace(".", File.separator)
            + File.separator +"cucumber",
            serviceName + "Test.java"
        ));
        
        this.supportingFiles.add(new SupportingFile(
                "cucumber" + File.separator + "cucumberSteps.mustache",
                this.testFolder + File.separator + this.basePackage .replace(".", File.separator)
                + File.separator +"cucumber",
                serviceName + "Steps.java"
            ));
        
        this.supportingFiles.add(new SupportingFile(
                "cucumber" + File.separator + "package.mustache",
                this.testFolder + File.separator + this.basePackage .replace(".", File.separator)
                + File.separator +"cucumber",
                serviceName + "package-info.java"
            ));
        
  //test resources
        this.supportingFiles.add(new SupportingFile(
                "cucumber" + File.separator + "cucumber.mustache",
                (("src.test.resources") + File.separator + this.basePackage).replace(".", File.separator)
                    + File.separator + "cucumber",
                serviceName+".feature"
            ));
        
        this.supportingFiles.add(new SupportingFile(
                "testresources" + File.separator + "bootstrap.mustache",
                ("src.test.resources").replace(".", java.io.File.separator),
                "bootstrap.yml"));
        this.supportingFiles.add(new SupportingFile(
                "testresources" + File.separator + "application.mustache",
                ("src.test.resources").replace(".", java.io.File.separator),
                "application.properties"));
        this.supportingFiles.add(new SupportingFile(
                "testresources" + File.separator + "application-test.mustache",
                ("src.test.resources").replace(".", java.io.File.separator),
                "application-test.properties"));
        
 //Gatling
        this.supportingFiles.add(new SupportingFile(
                "gatling" + File.separator + "gatling.mustache",
                ("src.test.resources").replace(".", java.io.File.separator),
                "gatling.conf"));
        
        
        this.supportingFiles.add(new SupportingFile(
                "gatling" + File.separator + "application.mustache",
                ("src.test.resources").replace(".", java.io.File.separator),
                "application.conf"));
        
        this.supportingFiles.add(new SupportingFile(
                "gatling" + File.separator + "testapi.mustache",
                ("src")+ File.separator + ("test")+ File.separator + ("scala")+ File.separator + ("scalaFiles").replace(".", java.io.File.separator),
                "testapi.scala"));
        
        /*this.supportingFiles.add(new SupportingFile(
                "gatling" + File.separator + "package.mustache",
                ("src")+ File.separator + ("test")+ File.separator + ("scala")+ File.separator + ("scalaFiles").replace(".", java.io.File.separator),
                "package.info"));*/
        
   // adding class for integration test
        this.supportingFiles.add(new SupportingFile(
                "integration" + File.separator + "integrationtest.mustache",
                this.testFolder + File.separator + this.basePackage .replace(".", File.separator)
                + File.separator +"controller",
                serviceName + "IT.java"
            ));  
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
    if (operations != null) {
      List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
      for (final CodegenOperation operation : ops) {
        List<CodegenResponse> responses = operation.responses;
        if (responses != null) {
          for (final CodegenResponse resp : responses) {
            if ("0".equals(resp.code)) {
              resp.code = "200";
            }
            doDataTypeAssignment(resp.dataType, new DataTypeAssigner() {

              public void setReturnType(final String returnType) {
                resp.dataType = returnType;
              }

              public void setReturnContainer(final String returnContainer) {
                resp.containerType = returnContainer;
              }
            });
          }
        }

        doDataTypeAssignment(operation.returnType, new DataTypeAssigner() {

          public void setReturnType(final String returnType) {
            operation.returnType = returnType;
          }

          public void setReturnContainer(final String returnContainer) {
            operation.returnContainer = returnContainer;
          }
        });

        if (implicitHeaders) {
          removeHeadersFromAllParams(operation.allParams);
        }
      }
    }

    return objs;
  }

  /**
   * This method removes header parameters from the list of parameters and also
   * corrects last allParams hasMore state.
   *
   * @param allParams list of all parameters
   */
  private void removeHeadersFromAllParams(List<CodegenParameter> allParams) {
    if (allParams.isEmpty()) {
      return;
    }
    final ArrayList<CodegenParameter> copy = new ArrayList(allParams);
    allParams.clear();

    for (CodegenParameter p : copy) {
      if (!p.isHeaderParam) {
        allParams.add(p);
      }
    }
    allParams.get(allParams.size() - 1).hasMore = false;
  }

  /**
   * @param returnType The return type that needs to be converted
   * @param dataTypeAssigner An object that will assign the data to the respective fields in the
   * model.
   */
  private void doDataTypeAssignment(String returnType, DataTypeAssigner dataTypeAssigner) {
    final String rt = returnType;
    if (rt == null) {
      dataTypeAssigner.setReturnType("Void");
    } else if (rt.startsWith("List")) {
      int end = rt.lastIndexOf(">");
      if (end > 0) {
        dataTypeAssigner.setReturnType(rt.substring("List<".length(), end).trim());
        dataTypeAssigner.setReturnContainer("List");
      }
    } else if (rt.startsWith("Map")) {
      int end = rt.lastIndexOf(">");
      if (end > 0) {
        dataTypeAssigner.setReturnType(rt.substring("Map<".length(), end).split(",")[1].trim());
        dataTypeAssigner.setReturnContainer("Map");
      }
    } else if (rt.startsWith("Set")) {
      int end = rt.lastIndexOf(">");
      if (end > 0) {
        dataTypeAssigner.setReturnType(rt.substring("Set<".length(), end).trim());
        dataTypeAssigner.setReturnContainer("Set");
      }
    }
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);

    swaggerType = getAlias(swaggerType);

    // don't apply renaming on types from the typeMapping
    if (this.typeMapping.containsKey(swaggerType)) {
      return this.typeMapping.get(swaggerType);
    }

    if (null == swaggerType) {
      LOGGER.error("No Type defined for Property " + p);
    }
    return toModelName(swaggerType);
  }

  @Override
  public String getAlias(String name) {
    if (typeAliases.containsKey(name)) {
      return typeAliases.get(name);
    }
    return name;
  }

  @Override
  public String toModelName(final String name) {
    // We need to check if import-mapping has a different model for this class, so we use it
    // instead of the auto-generated one.
    if (this.importMapping.containsKey(name)) {
      return this.importMapping.get(name);
    }

    final String sanitizedName = sanitizeName(name);

    String nameWithPrefixSuffix = sanitizedName;
    if (!StringUtils.isEmpty(modelNamePrefix)) {
      // add '_' so that model name can be camelized correctly
      nameWithPrefixSuffix = modelNamePrefix + "_" + nameWithPrefixSuffix;
    }

    if (!StringUtils.isEmpty(modelNameSuffix)) {
      // add '_' so that model name can be camelized correctly
      nameWithPrefixSuffix = nameWithPrefixSuffix + "_" + modelNameSuffix;
    }

    // camelize the model name
    // phone_number => PhoneNumber
    final String camelizedName = camelize(nameWithPrefixSuffix);

    // model name cannot use reserved keyword, e.g. return
    if (isReservedWord(camelizedName)) {
      final String modelName = "Model" + camelizedName;
      LOGGER.warn(
          camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
      return modelName;
    }

    // model name starts with number
    if (camelizedName.matches("^\\d.*")) {
      final String modelName =
          "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
      LOGGER.warn(
          name + " (model name starts with number) cannot be used as model name. Renamed to "
              + modelName);
      return modelName;
    }

    return camelizedName;
  }

  @Override
  public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
    if (serializeBigDecimalAsString) {
      if (property.baseType.equals("BigDecimal")) {
        // we serialize BigDecimal as `string` to avoid precision loss
        property.vendorExtensions
            .put("extraAnnotation", "@JsonSerialize(using = ToStringSerializer.class)");

        // this requires some more imports to be added for this model...
        model.imports.add("ToStringSerializer");
        model.imports.add("JsonSerialize");
      }
    }

    if (!fullJavaUtil) {
      if ("array".equals(property.containerType)) {
        model.imports.add("ArrayList");
      } else if ("map".equals(property.containerType)) {
        model.imports.add("HashMap");
      }
    }

    if (!BooleanUtils.toBoolean(model.isEnum)) {
      // needed by all pojos, but not enums
      model.imports.add("ApiModelProperty");
      model.imports.add("ApiModel");
    }

    //super.postProcessModelProperty(model, property);

    if ("null".equals(property.example)) {
      property.example = null;
    }

    //Add imports for Jackson
    if (!Boolean.TRUE.equals(model.isEnum)) {
      model.imports.add("JsonProperty");

      if (Boolean.TRUE.equals(model.hasEnums)) {
        model.imports.add("JsonValue");
      }
    } else { // enum class
      //Needed imports for Jackson's JsonCreator
      if (this.additionalProperties.containsKey("jackson")) {
        model.imports.add("JsonCreator");
      }
    }
  }

  @Override
  public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
    objs = super.postProcessModelsEnum(objs);

    //Add imports for Jackson
    List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
    List<Object> models = (List<Object>) objs.get("models");
    for (Object _mo : models) {
      Map<String, Object> mo = (Map<String, Object>) _mo;
      CodegenModel cm = (CodegenModel) mo.get("model");
      // for enum model
      if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
        cm.imports.add(this.importMapping.get("JsonValue"));
        Map<String, String> item = new HashMap<String, String>();
        item.put("import", this.importMapping.get("JsonValue"));
        imports.add(item);
      }
    }

    return objs;
  }

  @Override
  public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
    CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);
    if (codegenModel.description != null) {
      codegenModel.imports.add("ApiModel");
    }
    if (codegenModel.discriminator != null && this.additionalProperties.containsKey("jackson")) {
      codegenModel.imports.add("JsonSubTypes");
      codegenModel.imports.add("JsonTypeInfo");
    }
    if (allDefinitions != null && codegenModel.parentSchema != null && codegenModel.hasEnums) {
      final Model parentModel = allDefinitions.get(codegenModel.parentSchema);
      final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent, parentModel);
      codegenModel = PkmstServerCodegen.reconcileInlineEnums(codegenModel, parentCodegenModel);
    }
    return codegenModel;
  }

  @Override
  public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
    return objs;
  }

  @Override
  public Map<String, Object> postProcessModels(Map<String, Object> objs) {
    // recursively add import for mapping one type to multiple imports
    List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
    if (recursiveImports == null) {
      return objs;
    }

    ListIterator<Map<String, String>> listIterator = recursiveImports.listIterator();
    while (listIterator.hasNext()) {
      String _import = listIterator.next().get("import");
      // if the import package happens to be found in the importMapping (key)
      // add the corresponding import package to the list
      if (importMapping.containsKey(_import)) {
        Map<String, String> newImportMap = new HashMap<String, String>();
        newImportMap.put("import", importMapping.get(_import));
        listIterator.add(newImportMap);
      }
    }

    return postProcessModelsEnum(objs);
  }

  @Override
  public void preprocessSwagger(Swagger swagger) {
	  super.preprocessSwagger(swagger);
    if (swagger == null || swagger.getPaths() == null) {
      return;
    }
    if(swagger.getTags()!=null){
    	System.out.println("Tags are::"+swagger.getTags());
    List<ResourcePath> resourcePaths = new ArrayList();
    for (Tag tag : swagger.getTags()) {
      ResourcePath resourcePath = new ResourcePath();
      resourcePath.setPath(tag.getName());
      resourcePaths.add(resourcePath);
    }
    this.additionalProperties.put("resourcePaths", resourcePaths);
    }
    //get vendor extensions
    
    Map<String,Object> vendorExt = swagger.getInfo().getVendorExtensions();
    if(vendorExt !=null && !vendorExt.toString().equals("")){
    	if(vendorExt.containsKey("x-codegen")){
    		
    		Map<String,String> uris = (Map<String, String>) vendorExt.get("x-codegen");
    		if(uris.containsKey("eurekaUri")){
    			String eurekaUri = uris.get("eurekaUri");
    			additionalProperties.put(EUREKA_URI,eurekaUri);
    		}
    		if(uris.containsKey("zipkinUri")){
    			String zipkinUri = uris.get("zipkinUri");
    			additionalProperties.put(ZIPKIN_URI, zipkinUri);
    		}
    		if(uris.containsKey("springBootAdminUri")){
    			String springBootAdminUri = uris.get("springBootAdminUri");
    			additionalProperties.put(SPRINGADMIN_URI, springBootAdminUri);
    		}
    		if(uris.containsKey("pkmstInterceptor")){
    			String pkmstInterceptor = uris.get("pkmstInterceptor");
    			additionalProperties.put("pkmstInterceptor", pkmstInterceptor);
    		}
    	}
    }
    
   
   
    for (String pathname : swagger.getPaths().keySet()) {
      Path path = swagger.getPath(pathname);
      if (path.getOperations() == null) {
        continue;
      }
      for (Operation operation : path.getOperations()) {
        boolean hasFormParameters = false;
        for (Parameter parameter : operation.getParameters()) {
          if (parameter instanceof FormParameter) {
            hasFormParameters = true;
          }
        }
        //only add content-Type if its no a GET-Method
        if (path.getGet() != null || !operation.equals(path.getGet())) {
          String defaultContentType =
              hasFormParameters ? "application/x-www-form-urlencoded" : "application/json";
          String contentType = operation.getConsumes() == null || operation.getConsumes().isEmpty()
              ? defaultContentType : operation.getConsumes().get(0);
          operation.setVendorExtension("x-contentType", contentType);
        }
        String accepts = getAccept(operation);
        operation.setVendorExtension("x-accepts", accepts);
      }
    }

    if ("/".equals(swagger.getBasePath())) {
      swagger.setBasePath("");
    }

    if (!additionalProperties.containsKey(TITLE)) {
      // From the title, compute a reasonable name for the package and the API
      String title = swagger.getInfo().getTitle();

      // Drop any API suffix
      if (title != null) {
        title = title.trim().replace(" ", "-");
        if (title.toUpperCase().endsWith("API")) {
          title = title.substring(0, title.length() - 3);
        }

        this.title = camelize(sanitizeName(title), true);
      }
      additionalProperties.put(TITLE, this.title);
    }

    String host = swagger.getHost();
    String port = "8008";
    if (host != null) {
      String[] parts = host.split(":");
      if (parts.length > 1) {
        port = parts[1];
      }
    }

    this.additionalProperties.put("serverPort", port);
    if (swagger.getPaths() != null) {
      for (String pathname : swagger.getPaths().keySet()) {
        Path path = swagger.getPath(pathname);
        if (path.getOperations() != null) {
          for (Operation operation : path.getOperations()) {
            if (operation.getTags() != null) {
              List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
              for (String tag : operation.getTags()) {
                Map<String, String> value = new HashMap<String, String>();
                value.put("tag", tag);
                value.put("hasMore", "true");
                tags.add(value);
              }
              if (tags.size() > 0) {
                tags.get(tags.size() - 1).remove("hasMore");
              }
              if (operation.getTags().size() > 0) {
                String tag = operation.getTags().get(0);
                operation.setTags(Arrays.asList(tag));
              }
              operation.setVendorExtension("x-tags", tags);
            }
          }
        }
      }
    }
  }

  @Override
  public void addOperationToGroup(String tag, String resourcePath, Operation operation,
      CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
    String basePath = resourcePath;
    if (basePath.startsWith("/")) {
      basePath = basePath.substring(1);
    }
    int pos = basePath.indexOf("/");
    if (pos > 0) {
      basePath = basePath.substring(0, pos);
    }

    if (basePath.equals("")) {
      basePath = "default";
    } else {
      co.subresourceOperation = !co.path.isEmpty();
    }
    List<CodegenOperation> opList = operations.get(basePath);
    if (opList == null) {
      opList = new ArrayList<CodegenOperation>();
      operations.put(basePath, opList);
    }
    opList.add(co);
    co.baseName = basePath;
  }

  @Override
  public String toApiName(String name) {
    if (name.length() == 0) {
      return "DefaultApi";
    }
    name = sanitizeName(name);
    return camelize(name) + "Api";
  }

  @Override
  public void setParameterExampleValue(CodegenParameter p) {
    String type = p.baseType;
    if (type == null) {
      type = p.dataType;
    }

    if ("File".equals(type)) {
      String example;

      if (p.defaultValue == null) {
        example = p.example;
      } else {
        example = p.defaultValue;
      }

      if (example == null) {
        example = "/path/to/file";
      }
      example =
          "new org.springframework.core.io.FileSystemResource(new java.io.File(\"" + escapeText(
              example) + "\"))";
      p.example = example;
    } else {
      super.setParameterExampleValue(p);
    }
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
    return this.outputFolder + "/" + this.sourceFolder + "/" + apiPackage().replace(".", "/");
  }

  @Override
  public String apiTestFileFolder() {
    return this.outputFolder + "/" + this.testFolder + "/" + apiPackage().replace(".", "/");
  }

  @Override
  public String modelFileFolder() {
    return this.outputFolder + "/" + this.sourceFolder + "/" + modelPackage().replace(".", "/");
  }

  @Override
  public String apiDocFileFolder() {
    return (this.outputFolder + "/" + this.apiDocPath).replace("/", File.separator);
  }

  @Override
  public String modelDocFileFolder() {
    return (this.outputFolder + "/" + this.modelDocPath).replace("/", File.separator);
  }

  @Override
  public String toVarName(String name) {
    // sanitize name
    name = sanitizeName(
        name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

    if (name.toLowerCase().matches("^_*class$")) {
      return "propertyClass";
    }

    if ("_".equals(name)) {
      name = "_u";
    }

    // if it's all uppper case, do nothing
    if (name.matches("^[A-Z_]*$")) {
      return name;
    }

    if (startsWithTwoUppercaseLetters(name)) {
      name = name.substring(0, 2).toLowerCase() + name.substring(2);
    }

    // camelize (lower first character) the variable name
    // pet_id => petId
    name = camelize(name, true);

    // for reserved word or word starting with number, append _
    if (isReservedWord(name) || name.matches("^\\d.*")) {
      name = escapeReservedWord(name);
    }

    return name;
  }

  @Override
  public String toParamName(String name) {
    // to avoid conflicts with 'callback' parameter for async call
    if ("callback".equals(name)) {
      return "paramCallback";
    }

    // should be the same as variable name
    return toVarName(name);
  }

  @Override
  public String toModelFilename(String name) {
    // should be the same as the model name
    return toModelName(name);
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      if (inner == null) {
        LOGGER.warn(ap.getName() + "(array property) does not have a proper inner type defined");
        // TODO maybe better defaulting to StringProperty than returning null
        return null;
      }
      return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
    } else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();
      if (inner == null) {
        LOGGER.warn(mp.getName() + "(map property) does not have a proper inner type defined");
        // TODO maybe better defaulting to StringProperty than returning null
        return null;
      }
      return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String toDefaultValue(Property p) {
    if (p instanceof ArrayProperty) {
      final ArrayProperty ap = (ArrayProperty) p;
      final String pattern;
      if (fullJavaUtil) {
        pattern = "new java.util.ArrayList<%s>()";
      } else {
        pattern = "new ArrayList<%s>()";
      }
      if (ap.getItems() == null) {
        return null;
      }

      return String.format(pattern, getTypeDeclaration(ap.getItems()));
    } else if (p instanceof MapProperty) {
      final MapProperty ap = (MapProperty) p;
      final String pattern;
      if (fullJavaUtil) {
        pattern = "new java.util.HashMap<%s>()";
      } else {
        pattern = "new HashMap<%s>()";
      }
      if (ap.getAdditionalProperties() == null) {
        return null;
      }

      return String.format(pattern,
          String.format("String, %s", getTypeDeclaration(ap.getAdditionalProperties())));
    } else if (p instanceof IntegerProperty) {
      IntegerProperty dp = (IntegerProperty) p;
      if (dp.getDefault() != null) {
        return dp.getDefault().toString();
      }
      return "null";
    } else if (p instanceof LongProperty) {
      LongProperty dp = (LongProperty) p;
      if (dp.getDefault() != null) {
        return dp.getDefault().toString() + "l";
      }
      return "null";
    } else if (p instanceof DoubleProperty) {
      DoubleProperty dp = (DoubleProperty) p;
      if (dp.getDefault() != null) {
        return dp.getDefault().toString() + "d";
      }
      return "null";
    } else if (p instanceof FloatProperty) {
      FloatProperty dp = (FloatProperty) p;
      if (dp.getDefault() != null) {
        return dp.getDefault().toString() + "f";
      }
      return "null";
    } else if (p instanceof BooleanProperty) {
      BooleanProperty bp = (BooleanProperty) p;
      if (bp.getDefault() != null) {
        return bp.getDefault().toString();
      }
      return "null";
    } else if (p instanceof StringProperty) {
      StringProperty sp = (StringProperty) p;
      if (sp.getDefault() != null) {
        String _default = sp.getDefault();
        if (sp.getEnum() == null) {
          return "\"" + escapeText(_default) + "\"";
        } else {
          // convert to enum var name later in postProcessModels
          return _default;
        }
      }
      return "null";
    }
    return super.toDefaultValue(p);
  }

  @Override
  public String toExampleValue(Property p) {
    if (p.getExample() != null) {
      return escapeText(p.getExample().toString());
    } else {
      return super.toExampleValue(p);
    }
  }

  @Override
  public String toOperationId(String operationId) {
    // throw exception if method name is empty
    if (StringUtils.isEmpty(operationId)) {
      throw new RuntimeException("Empty method/operation name (operationId) not allowed");
    }

    operationId = camelize(sanitizeName(operationId), true);

    // method name cannot use reserved keyword, e.g. return
    if (isReservedWord(operationId)) {
      String newOperationId = camelize("call_" + operationId, true);
      LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to "
          + newOperationId);
      return newOperationId;
    }

    return operationId;
  }

  @Override
  protected boolean needToImport(String type) {
    return super.needToImport(type) && type.indexOf(".") < 0;
  }

  @Override
  public String toEnumName(CodegenProperty property) {
    return sanitizeName(camelize(property.name)) + "Enum";
  }

  @Override
  public String toEnumVarName(String value, String datatype) {
    if (value.length() == 0) {
      return "EMPTY";
    }

    // for symbol, e.g. $, #
    if (getSymbolName(value) != null) {
      return getSymbolName(value).toUpperCase();
    }

    // number
    if ("Integer".equals(datatype) || "Long".equals(datatype) ||
        "Float".equals(datatype) || "Double".equals(datatype)) {
      String varName = "NUMBER_" + value;
      varName = varName.replace("-", "MINUS_");
      varName = varName.replace("\\+", "PLUS_");
      varName = varName.replace("\\.", "_DOT_");
      return varName;
    }

    // string
    String var = value.replace("\\W+", "_").toUpperCase();
    if (var.matches("\\d.*")) {
      return "_" + var;
    } else {
      return var;
    }
  }

  @Override
  public String toEnumValue(String value, String datatype) {
    if ("Integer".equals(datatype) || "Long".equals(datatype) ||
        "Double".equals(datatype)) {
      return value;
    } else if ("Float".equals(datatype)) {
      // add f to number, e.g. 3.14 => 3.14f
      return value + "f";
    } else {
      return "\"" + escapeText(value) + "\"";
    }
  }

  @Override
  public CodegenOperation fromOperation(String path, String httpMethod, Operation operation,
      Map<String, Model> definitions, Swagger swagger) {
    CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);
    op.path = sanitizePath(op.path);
    return op;
  }

  @Override
  public String escapeQuotationMark(String input) {
    // remove " to avoid code injection
    return input.replace("\"", "");
  }

  @Override
  public String escapeUnsafeCharacters(String input) {
    return input.replace("*/", "*_/").replace("/*", "/_*");
  }

  public boolean convertPropertyToBoolean(String propertyKey) {
    boolean booleanValue = false;
    if (additionalProperties.containsKey(propertyKey)) {
      booleanValue = Boolean.valueOf(additionalProperties.get(propertyKey).toString());
    }

    return booleanValue;
  }

  public String toRegularExpression(String pattern) {
    return escapeText(pattern);
  }

  @Override
  public String sanitizeTag(String tag) {
    return camelize(sanitizeName(tag));
  }

  public String toBooleanGetter(String name) {
    return "is" + getterAndSetterCapitalize(name);
  }

  public String getGroupId() {
    return groupId;
  }

  public void setGroupId(String groupId) {
    this.groupId = groupId;
  }

  public String getArtifactId() {
    return artifactId;
  }

  public void setArtifactId(String artifactId) {
    this.artifactId = artifactId;
  }

  public String getArtifactVersion() {
    return artifactVersion;
  }

  public void setArtifactVersion(String artifactVersion) {
    this.artifactVersion = artifactVersion;
  }
  

  public String getProjectFolder() {
    return projectFolder;
  }

  public void setProjectFolder(String projectFolder) {
    this.projectFolder = projectFolder;
  }

  public String getEurekaUri() {
		return eurekaUri;
	}

	public void setEurekaUri(String eurekaUri) {
		this.eurekaUri = eurekaUri;
	}

	public String getZipkinUri() {
		return zipkinUri;
	}

	public void setZipkinUri(String zipkinUri) {
		this.zipkinUri = zipkinUri;
	}

	public String getSpringBootAdminUri() {
		return springBootAdminUri;
	}

	public void setSpringBootAdminUri(String springBootAdminUri) {
		this.springBootAdminUri = springBootAdminUri;
	}
  public String getProjectTestFolder() {
    return projectTestFolder;
  }

  public void setProjectTestFolder(String projectTestFolder) {
    this.projectTestFolder = projectTestFolder;
  }

  public String getSourceFolder() {
    return sourceFolder;
  }

  public void setSourceFolder(String sourceFolder) {
    this.sourceFolder = sourceFolder;
  }

  public String getTestFolder() {
    return testFolder;
  }

  public void setTestFolder(String testFolder) {
    this.testFolder = testFolder;
  }

  public String getBasePackage() {
    return basePackage;
  }

  public void setBasePackage(String basePackage) {
    this.basePackage = basePackage;
  }

  public String getServiceName() {
    return serviceName;
  }

  public void setServiceName(String serviceName) {
    this.serviceName = serviceName;
  }

  public String getConfigPackage() {
    return configPackage;
  }

  public void setConfigPackage(String configPackage) {
    this.configPackage = configPackage;
  }

  public boolean isImplicitHeaders() {
    return implicitHeaders;
  }

  public void setImplicitHeaders(boolean implicitHeaders) {
    this.implicitHeaders = implicitHeaders;
  }

  public boolean isSerializeBigDecimalAsString() {
    return serializeBigDecimalAsString;
  }

  public void setSerializeBigDecimalAsString(boolean serializeBigDecimalAsString) {
    this.serializeBigDecimalAsString = serializeBigDecimalAsString;
  }

  public boolean isFullJavaUtil() {
    return fullJavaUtil;
  }

  public void setFullJavaUtil(boolean fullJavaUtil) {
    this.fullJavaUtil = fullJavaUtil;
  }

  public Boolean getSerializableModel() {
    return serializableModel;
  }

  public void setSerializableModel(Boolean serializableModel) {
    this.serializableModel = serializableModel;
  }

  public String getInvokerPackage() {
    return invokerPackage;
  }

  public void setInvokerPackage(String invokerPackage) {
    this.invokerPackage = invokerPackage;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public boolean isWithXml() {
    return withXml;
  }

  public void setWithXml(boolean withXml) {
    this.withXml = withXml;
  }

  private boolean startsWithTwoUppercaseLetters(String name) {
    boolean startsWithTwoUppercaseLetters = false;
    if (name.length() > 1) {
      startsWithTwoUppercaseLetters = name.substring(0, 2)
          .equals(name.substring(0, 2).toUpperCase());
    }
    return startsWithTwoUppercaseLetters;
  }

  private String sanitizePath(String p) {
    //prefer replace a ", instead of a fuLL URL encode for readability
    return p.replace("\"", "%22");
  }

  private interface DataTypeAssigner {

    void setReturnType(String returnType);

    void setReturnContainer(String returnContainer);
  }

  private class ResourcePath {

    private String path;

    public String getPath() {
      return path;
    }

    public void setPath(String path) {
      this.path = path;
    }

    @Override
    public String toString() {
      return this.path;
    }
  }
}
