package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class CSharpClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_csharp";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String PACKAGE_TITLE_VALUE = "Swagger library";
    public static final String PACKAGE_DESCRIPTION_VALUE = "A library generated from a Swagger doc";
    public static final String PACKAGE_CONFIGURATION_VALUE = "No Configuration";
    public static final String PACKAGE_COMPANY_VALUE = "Swagger";
    public static final String PACKAGE_PRODUCT_NAME_VALUE = "SwaggerLibrary";
    public static final String PACKAGE_COPYRIGHT_VALUE = "No Copyright";
    public static final String PACKAGE_TRADEMARK_VALUE = "No Trademark";
    public static final String PACKAGE_CULTURE_VALUE = "en-us";
    public static final String SOURCE_FOLDER_VALUE = "src_csharp";
    public static final String PACKAGE_GUID_VALUE = "{894EAEBB-649A-498C-A735-10D0BD7B73E0}";
	
    @Override
    public String getLanguage() {
        return "csharp";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_TITLE, PACKAGE_TITLE_VALUE)
                .put(CodegenConstants.PACKAGE_DESCRIPTION, PACKAGE_DESCRIPTION_VALUE)
                .put(CodegenConstants.PACKAGE_CONFIGURATION, PACKAGE_CONFIGURATION_VALUE)
                .put(CodegenConstants.PACKAGE_COMPANY, PACKAGE_COMPANY_VALUE)
                .put(CodegenConstants.PACKAGE_PRODUCT_NAME, PACKAGE_PRODUCT_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_COPYRIGHT, PACKAGE_COPYRIGHT_VALUE)
                .put(CodegenConstants.PACKAGE_TRADEMARK, PACKAGE_TRADEMARK_VALUE)
                .put(CodegenConstants.PACKAGE_CULTURE, PACKAGE_CULTURE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(CodegenConstants.OPTIONAL_METHOD_ARGUMENT, "true")
                .put(CodegenConstants.OPTIONAL_ASSEMBLY_INFO, "true")
                .put(CodegenConstants.USE_DATETIME_OFFSET, "true")
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.USE_COLLECTION, "false")
                .put(CodegenConstants.RETURN_ICOLLECTION, "false")
                .put(CodegenConstants.OPTIONAL_PROJECT_FILE, "true")
                .put(CodegenConstants.OPTIONAL_PROJECT_GUID, PACKAGE_GUID_VALUE)
                .put(CodegenConstants.DOTNET_FRAMEWORK, "4.x")
                .put(CodegenConstants.OPTIONAL_EMIT_DEFAULT_VALUES, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
