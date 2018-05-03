package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Model;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.Collator;
import java.util.*;

import static io.swagger.codegen.languages.JavaClientCodegen.prioritizeContentTypes;
import static java.util.Collections.sort;

public class KotlinClientCodegen extends AbstractKotlinCodegen {

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String USE_ASYNC = "useAsync";
    private static final String RETROFIT_2 = "retrofit2";

    static Logger LOGGER = LoggerFactory.getLogger(KotlinClientCodegen.class);

    protected String dateLibrary = DateLibrary.JAVA8_LOCAL.value;

    public enum DateLibrary {
        STRING("string"),
        THREETENBP("threetenbp"),
        JAVA8_LOCAL("java8-localdatetime"),
        JAVA8("java8");

        public final String value;

        DateLibrary(String value) {
            this.value = value;
        }
    }

    /**
     * Constructs an instance of `KotlinClientCodegen`.
     */
    public KotlinClientCodegen() {
        super();

        artifactId = "kotlin-client";
        packageName = "io.swagger.client";

        outputFolder = "generated-code" + File.separator + "kotlin-client";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "kotlin-client";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DateLibrary.THREETENBP.value, "Threetenbp");
        dateOptions.put(DateLibrary.STRING.value, "String");
        dateOptions.put(DateLibrary.JAVA8.value, "Java 8 native JSR310");
        dateOptions.put(DateLibrary.JAVA8_LOCAL.value, "Java 8 using LocalDateTime");
        dateLibrary.setEnum(dateOptions);
        cliOptions.add(dateLibrary);
        cliOptions.add(CliOption.newBoolean(USE_ASYNC, "Whether to use the Kotlin coroutines with the retrofit2 library."));

        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 3.8.0. JSON processing: Gson 2.6.1 (Retrofit 2.3.0).");
        if (usesRetrofit2Library()) {
            instantiationTypes.put("array", "kotlin.collections.ArrayList");
            instantiationTypes.put("list", "kotlin.collections.ArrayList");
        }
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "kotlin";
    }

    public String getHelp() {
        return "Generates a kotlin client.";
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());
        }

        if (DateLibrary.THREETENBP.value.equals(dateLibrary)) {
            additionalProperties.put(DateLibrary.THREETENBP.value, true);
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "LocalDateTime");
            importMapping.put("LocalDate", "org.threeten.bp.LocalDate");
            importMapping.put("LocalDateTime", "org.threeten.bp.LocalDateTime");
            defaultIncludes.add("org.threeten.bp.LocalDateTime");
        } else if (DateLibrary.STRING.value.equals(dateLibrary)) {
            typeMapping.put("date-time", "kotlin.String");
            typeMapping.put("date", "kotlin.String");
            typeMapping.put("Date", "kotlin.String");
            typeMapping.put("DateTime", "kotlin.String");
        } else if (DateLibrary.JAVA8_LOCAL.value.equals(dateLibrary)) {
            additionalProperties.put(DateLibrary.JAVA8.value, true);
        } else if (DateLibrary.JAVA8.value.equals(dateLibrary)) {
            typeMapping.put("date-time", "java.time.OffsetDateTime");
            typeMapping.put("date", "java.time.OffsetDateTime");
            typeMapping.put("Date", "java.time.OffsetDateTime");
            typeMapping.put("DateTime", "java.time.OffsetDateTime");
            additionalProperties.put(DateLibrary.JAVA8.value, true);
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", "/");

        supportingFiles.add(new SupportingFile("infrastructure/ApiClient.kt.mustache", infrastructureFolder, "ApiClient.kt"));
        if (!usesRetrofit2Library()) {
            supportingFiles.add(new SupportingFile("infrastructure/ApiAbstractions.kt.mustache", infrastructureFolder, "ApiAbstractions.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/ApiInfrastructureResponse.kt.mustache", infrastructureFolder, "ApiInfrastructureResponse.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/ApplicationDelegates.kt.mustache", infrastructureFolder, "ApplicationDelegates.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/RequestConfig.kt.mustache", infrastructureFolder, "RequestConfig.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/RequestMethod.kt.mustache", infrastructureFolder, "RequestMethod.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/ResponseExtensions.kt.mustache", infrastructureFolder, "ResponseExtensions.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/Serializer.kt.mustache", infrastructureFolder, "Serializer.kt"));
            supportingFiles.add(new SupportingFile("infrastructure/Errors.kt.mustache", infrastructureFolder, "Errors.kt"));
        }
        if (usesRetrofit2Library()) {
            typeMapping.put("array", "kotlin.collections.List");
            supportingFiles.add(new SupportingFile("infrastructure/JSON.kt.mustache", infrastructureFolder, "JSON.kt"));
            apiTestTemplateFiles.put("api_test.mustache",".kt");
        }

    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        super.postProcessOperations(objs);
        if (usesRetrofit2Library()) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.hasConsumes == Boolean.TRUE) {

                        if (isMultipartType(operation.consumes)) {
                            operation.isMultipart = Boolean.TRUE;
                        }
                        else {
                            operation.prioritizedContentTypes = prioritizeContentTypes(operation.consumes);
                        }
                    }
                    if (StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/")){
                        operation.path = operation.path.substring(1);
                    }

                    // sorting operation parameters to make sure path params are parsed before query params
                    if (operation.allParams != null) {
                        sort(operation.allParams, new Comparator<CodegenParameter>() {
                            @Override
                            public int compare(CodegenParameter one, CodegenParameter another) {
                                if (one.isPathParam && another.isQueryParam) {
                                    return -1;
                                }
                                if (one.isQueryParam && another.isPathParam){
                                    return 1;
                                }

                                return 0;
                            }
                        });
                        Iterator<CodegenParameter> iterator = operation.allParams.iterator();
                        while (iterator.hasNext()){
                            CodegenParameter param = iterator.next();
                            param.hasMore = iterator.hasNext();
                        }
                    }
                }
            }

        }
        return objs;
    }

    private boolean usesRetrofit2Library() {
        return getLibrary() != null && getLibrary().contains("retrofit2");
    }

    private static boolean isMultipartType(List<Map<String, String>> consumes) {
        Map<String, String> firstType = consumes.get(0);
        if (firstType != null) {
            if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                return true;
            }
        }
        return false;
    }

}
