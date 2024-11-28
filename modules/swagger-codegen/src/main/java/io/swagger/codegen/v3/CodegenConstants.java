package io.swagger.codegen.v3;

import static io.swagger.codegen.v3.VendorExtendable.PREFIX_HAS;
import static io.swagger.codegen.v3.VendorExtendable.PREFIX_IS;

/**
 * A class for storing constants that are used throughout the project.
 */
public class CodegenConstants {
    /* System Properties */
    // NOTE: We may want to move these to a separate class to avoid confusion or modification.
    public static final String APIS = "apis";
    public static final String MODELS = "models";
    public static final String SUPPORTING_FILES = "supportingFiles";
    public static final String MODEL_TESTS = "modelTests";
    public static final String MODEL_DOCS = "modelDocs";
    public static final String API_TESTS = "apiTests";
    public static final String API_DOCS = "apiDocs";
    public static final String WITH_XML = "withXml";
    /* /end System Properties */

    public static final String API_PACKAGE = "apiPackage";
    public static final String API_PACKAGE_DESC = "package for generated api classes";

    public static final String MODEL_PACKAGE = "modelPackage";
    public static final String MODEL_PACKAGE_DESC = "package for generated models";

    public static final String TEMPLATE_DIR = "templateDir";
    public static final String TEMPLATE_VERSION = "templateVersion";

    public static final String ALLOW_UNICODE_IDENTIFIERS = "allowUnicodeIdentifiers";
    public static final String ALLOW_UNICODE_IDENTIFIERS_DESC = "boolean, toggles whether unicode identifiers are allowed in names or not, default is false";

    public static final String INVOKER_PACKAGE = "invokerPackage";
    public static final String INVOKER_PACKAGE_DESC = "root package for generated code";

    public static final String PHP_INVOKER_PACKAGE = "phpInvokerPackage";
    public static final String PHP_INVOKER_PACKAGE_DESC = "root package for generated php code";

    public static final String PERL_MODULE_NAME = "perlModuleName";
    public static final String PERL_MODULE_NAME_DESC = "root module name for generated perl code";

    public static final String PYTHON_PACKAGE_NAME = "pythonPackageName";
    public static final String PYTHON_PACKAGE_NAME_DESC = "package name for generated python code";

    public static final String GROUP_ID = "groupId";
    public static final String GROUP_ID_DESC = "groupId in generated pom.xml";

    public static final String ARTIFACT_ID = "artifactId";
    public static final String ARTIFACT_ID_DESC = "artifactId in generated pom.xml";

    public static final String ARTIFACT_VERSION = "artifactVersion";
    public static final String ARTIFACT_VERSION_DESC = "artifact version in generated pom.xml";

    public static final String ARTIFACT_URL = "artifactUrl";
    public static final String ARTIFACT_URL_DESC = "artifact URL in generated pom.xml";

    public static final String ARTIFACT_DESCRIPTION = "artifactDescription";
    public static final String ARTIFACT_DESCRIPTION_DESC = "artifact description in generated pom.xml";

    public static final String SCM_CONNECTION = "scmConnection";
    public static final String SCM_CONNECTION_DESC = "SCM connection in generated pom.xml";

    public static final String SCM_DEVELOPER_CONNECTION = "scmDeveloperConnection";
    public static final String SCM_DEVELOPER_CONNECTION_DESC = "SCM developer connection in generated pom.xml";

    public static final String SCM_URL = "scmUrl";
    public static final String SCM_URL_DESC = "SCM URL in generated pom.xml";

    public static final String DEVELOPER_NAME = "developerName";
    public static final String DEVELOPER_NAME_DESC = "developer name in generated pom.xml";

    public static final String DEVELOPER_EMAIL = "developerEmail";
    public static final String DEVELOPER_EMAIL_DESC = "developer email in generated pom.xml";

    public static final String DEVELOPER_ORGANIZATION = "developerOrganization";
    public static final String DEVELOPER_ORGANIZATION_DESC = "developer organization in generated pom.xml";

    public static final String DEVELOPER_ORGANIZATION_URL = "developerOrganizationUrl";
    public static final String DEVELOPER_ORGANIZATION_URL_DESC = "developer organization URL in generated pom.xml";

    public static final String LICENSE_NAME = "licenseName";
    public static final String LICENSE_NAME_DESC = "The name of the license";

    public static final String LICENSE_URL = "licenseUrl";
    public static final String LICENSE_URL_DESC = "The URL of the license";

    public static final String SOURCE_FOLDER = "sourceFolder";
    public static final String SOURCE_FOLDER_DESC = "source folder for generated code";

    public static final String IMPL_FOLDER = "implFolder";
    public static final String IMPL_FOLDER_DESC = "folder for generated implementation code";

    public static final String LOCAL_VARIABLE_PREFIX = "localVariablePrefix";
    public static final String LOCAL_VARIABLE_PREFIX_DESC = "prefix for generated code members and local variables";

    public static final String SERIALIZABLE_MODEL = "serializableModel";
    public static final String SERIALIZABLE_MODEL_DESC = "boolean - toggle \"implements Serializable\" for generated models";

    public static final String SERIALIZE_BIG_DECIMAL_AS_STRING = "bigDecimalAsString";
    public static final String SERIALIZE_BIG_DECIMAL_AS_STRING_DESC = "Treat BigDecimal values as Strings to avoid precision loss.";

    public static final String LIBRARY = "library";
    public static final String LIBRARY_DESC = "library template (sub-template)";

    public static final String SORT_PARAMS_BY_REQUIRED_FLAG = "sortParamsByRequiredFlag";
    public static final String SORT_PARAMS_BY_REQUIRED_FLAG_DESC = "Sort method arguments to place required parameters before optional parameters.";

    public static final String USE_DATETIME_OFFSET = "useDateTimeOffset";
    public static final String USE_DATETIME_OFFSET_DESC = "Use DateTimeOffset to model date-time properties";

    public static final String ENSURE_UNIQUE_PARAMS = "ensureUniqueParams";
    public static final String ENSURE_UNIQUE_PARAMS_DESC = "Whether to ensure parameter names are unique in an operation (rename parameters that are not).";

    public static final String PROJECT_NAME = "projectName";
    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";

    public static final String PACKAGE_TITLE = "packageTitle";
    public static final String PACKAGE_TITLE_DESC = "Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_PRODUCTNAME = "packageProductName";
    public static final String PACKAGE_PRODUCTNAME_DESC = "Specifies an AssemblyProduct for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_DESCRIPTION = "packageDescription";
    public static final String PACKAGE_DESCRIPTION_DESC = "Specifies a AssemblyDescription for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_COMPANY = "packageCompany";
    public static final String PACKAGE_COMPANY_DESC = "Specifies an AssemblyCompany for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_AUTHORS = "packageAuthors";
    public static final String PACKAGE_AUTHORS_DESC = "Specifies Authors property in the .NET Core project file.";
    public static final String PACKAGE_COPYRIGHT = "packageCopyright";
    public static final String PACKAGE_COPYRIGHT_DESC = "Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";

    public static final String POD_VERSION = "podVersion";

    public static final String OPTIONAL_METHOD_ARGUMENT = "optionalMethodArgument";
    public static final String OPTIONAL_METHOD_ARGUMENT_DESC = "Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).";

    public static final String OPTIONAL_ASSEMBLY_INFO = "optionalAssemblyInfo";
    public static final String OPTIONAL_ASSEMBLY_INFO_DESC = "Generate AssemblyInfo.cs.";

    public static final String NETCORE_PROJECT_FILE = "netCoreProjectFile";
    public static final String NETCORE_PROJECT_FILE_DESC = "Use the new format (.NET Core) for .NET project files (.csproj).";

    public static final String USE_COLLECTION = "useCollection";
    public static final String USE_COLLECTION_DESC = "Deserialize array types to Collection<T> instead of List<T>.";

    public static final String INTERFACE_PREFIX = "interfacePrefix";
    public static final String INTERFACE_PREFIX_DESC = "Prefix interfaces with a community standard or widely accepted prefix.";

    public static final String RETURN_ICOLLECTION = "returnICollection";
    public static final String RETURN_ICOLLECTION_DESC = "Return ICollection<T> instead of the concrete type.";

    public static final String OPTIONAL_PROJECT_FILE = "optionalProjectFile";
    public static final String OPTIONAL_PROJECT_FILE_DESC = "Generate {PackageName}.csproj.";

    public static final String OPTIONAL_PROJECT_GUID = "packageGuid";
    public static final String OPTIONAL_PROJECT_GUID_DESC = "The GUID that will be associated with the C# project";

    public static final String MODEL_PROPERTY_NAMING = "modelPropertyNaming";
    public static final String MODEL_PROPERTY_NAMING_DESC = "Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name";

    public static final String DOTNET_FRAMEWORK = "targetFramework";
    public static final String DOTNET_FRAMEWORK_DESC = "The target .NET framework version.";

    public static enum MODEL_PROPERTY_NAMING_TYPE {camelCase, PascalCase, snake_case, kebab_case, original}
    public static enum ENUM_PROPERTY_NAMING_TYPE {camelCase, PascalCase, snake_case, kebab_case, original, UPPERCASE}

    public static final String ENUM_PROPERTY_NAMING = "enumPropertyNaming";
    public static final String ENUM_PROPERTY_NAMING_DESC = "Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'";

    public static final String MODEL_NAME_PREFIX = "modelNamePrefix";
    public static final String MODEL_NAME_PREFIX_DESC = "Prefix that will be prepended to all model names. Default is the empty string.";

    public static final String MODEL_NAME_SUFFIX = "modelNameSuffix";
    public static final String MODEL_NAME_SUFFIX_DESC = "Suffix that will be appended to all model names. Default is the empty string.";

    public static final String OPTIONAL_EMIT_DEFAULT_VALUES = "optionalEmitDefaultValues";
    public static final String OPTIONAL_EMIT_DEFAULT_VALUES_DESC = "Set DataMember's EmitDefaultValue.";

    public static final String GIT_USER_ID = "gitUserId";
    public static final String GIT_USER_ID_DESC = "Git user ID, e.g. swagger-api.";

    public static final String GIT_REPO_ID = "gitRepoId";
    public static final String GIT_REPO_ID_DESC = "Git repo ID, e.g. swagger-codegen.";

    public static final String GIT_REPO_BASE_URL = "gitRepoBaseURL";
    public static final String GIT_REPO_BASE_URL_DESC = "Git repo Base URL, e.g. swagger-codegen.";

    public static final String RELEASE_NOTE = "releaseNote";
    public static final String RELEASE_NOTE_DESC = "Release note, default to 'Minor update'.";

    public static final String HTTP_USER_AGENT = "httpUserAgent";
    public static final String HTTP_USER_AGENT_DESC = "HTTP user agent, e.g. codegen_csharp_api_client, default to 'Swagger-Codegen/{packageVersion}}/{language}'";

    public static final String SUPPORTS_ES6 = "supportsES6";
    public static final String SUPPORTS_ES6_DESC = "Generate code that conforms to ES6.";

    public static final String SUPPORTS_ASYNC = "supportsAsync";
    public static final String SUPPORTS_ASYNC_DESC = "Generate code that supports async operations.";

    public static final String EXCLUDE_TESTS = "excludeTests";
    public static final String EXCLUDE_TESTS_DESC = "Specifies that no tests are to be generated.";

    // Not user-configurable. System provided for use in templates.
    public static final String GENERATE_APIS = "generateApis";
    public static final String GENERATE_API_DOCS = "generateApiDocs";

    public static final String GENERATE_API_TESTS = "generateApiTests";
    public static final String GENERATE_API_TESTS_DESC = "Specifies that api tests are to be generated.";

    // Not user-configurable. System provided for use in templates.
    public static final String GENERATE_MODELS = "generateModels";
    public static final String GENERATE_MODEL_DOCS = "generateModelDocs";

    public static final String GENERATE_MODEL_TESTS = "generateModelTests";
    public static final String GENERATE_MODEL_TESTS_DESC = "Specifies that model tests are to be generated.";

    public static final String HIDE_GENERATION_TIMESTAMP = "hideGenerationTimestamp";
    public static final String HIDE_GENERATION_TIMESTAMP_DESC = "Hides the generation timestamp when files are generated.";

    public static final String GENERATE_PROPERTY_CHANGED = "generatePropertyChanged";
    public static final String GENERATE_PROPERTY_CHANGED_DESC = "Specifies that models support raising property changed events.";

    public static final String PRESERVE_COMMENT_NEWLINES = "preserveNewlinesInComments";

    public static final String NON_PUBLIC_API = "nonPublicApi";
    public static final String NON_PUBLIC_API_DESC = "Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.";

    public static final String VALIDATABLE = "validatable";
    public static final String VALIDATABLE_DESC = "Generates self-validatable models.";

    public static final String IGNORE_FILE_OVERRIDE = "ignoreFileOverride";
    public static final String IGNORE_FILE_OVERRIDE_DESC = "Specifies an override location for the .swagger-codegen-ignore file. Most useful on initial generation.";

    public static final String REMOVE_OPERATION_ID_PREFIX = "removeOperationIdPrefix";
    public static final String REMOVE_OPERATION_ID_PREFIX_DESC = "Remove prefix of operationId, e.g. config_getId => getId";
    
    public static final String USE_OAS2 = "useOas2";
    public static final String USE_OAS2_DESC = "use OpenAPI v2.0 (Swagger 1.5.x)";

    public static final String TEMPLATE_ENGINE = "templateEngine";
    public static final String DISABLE_EXAMPLES_OPTION = "disableExample";
    public static final String IGNORE_IMPORT_MAPPING_OPTION = "ignoreImportMappings";

    public static final String IS_ENUM_EXT_NAME = PREFIX_IS + "enum";
    public static final String IS_ALIAS_EXT_NAME = PREFIX_IS + "alias";
    public static final String IS_ARRAY_MODEL_EXT_NAME = PREFIX_IS + "array-model";
    public static final String HAS_VARS_EXT_NAME = PREFIX_HAS + "vars";
    public static final String HAS_ENUMS_EXT_NAME = PREFIX_HAS + "enums";
    public static final String HAS_MORE_MODELS_EXT_NAME = PREFIX_HAS + "more-models";
    public static final String HAS_REQUIRED_EXT_NAME = PREFIX_HAS + "required";
    public static final String HAS_OPTIONAL_EXT_NAME = PREFIX_HAS + "optional";
    public static final String HAS_CHILDREN_EXT_NAME = PREFIX_HAS + "children";
    public static final String HAS_ONLY_READ_ONLY_EXT_NAME = PREFIX_HAS + "only-read-only";
    public static final String HAS_INNER_OBJECT_NAME = PREFIX_HAS + "inner-object";

    public static final String IS_SIMPLE_TYPE_EXT_NAME = PREFIX_IS + "simple-type";
    public static final String IS_NULLABLE_EXT_NAME = PREFIX_IS + "nullable";
    public static final String IS_PRIMITIVE_TYPE_EXT_NAME = PREFIX_IS + "primitive-type";
    public static final String IS_OBJECT_EXT_NAME = PREFIX_IS + "object";
    public static final String IS_CONTAINER_EXT_NAME = PREFIX_IS + "container";
    public static final String IS_NOT_CONTAINER_EXT_NAME = PREFIX_IS + "not-container";
    public static final String IS_DEFAULT_EXT_NAME = PREFIX_IS + "default";
    public static final String IS_STRING_EXT_NAME = PREFIX_IS + "string";
    public static final String IS_NUMERIC_EXT_NAME = PREFIX_IS + "numeric";
    public static final String IS_INTEGER_EXT_NAME = PREFIX_IS + "integer";
    public static final String IS_LONG_EXT_NAME = PREFIX_IS + "long";
    public static final String IS_NUMBER_EXT_NAME = PREFIX_IS + "number";
    public static final String IS_FLOAT_EXT_NAME = PREFIX_IS + "float";
    public static final String IS_DOUBLE_EXT_NAME = PREFIX_IS + "double";
    public static final String IS_BYTE_ARRAY_EXT_NAME = PREFIX_IS + "byte-array";
    public static final String IS_BINARY_EXT_NAME = PREFIX_IS + "binary";
    public static final String IS_FILE_EXT_NAME = PREFIX_IS + "file";
    public static final String IS_BOOLEAN_EXT_NAME = PREFIX_IS + "boolean";
    public static final String IS_DATE_EXT_NAME = PREFIX_IS + "date";
    public static final String IS_DATE_TIME_EXT_NAME = PREFIX_IS + "date-time";
    public static final String IS_UUID_EXT_NAME = PREFIX_IS + "uuid";
    public static final String IS_LIST_CONTAINER_EXT_NAME = PREFIX_IS + "list-container";
    public static final String IS_MAP_CONTAINER_EXT_NAME = PREFIX_IS + "map-container";
    public static final String IS_READ_ONLY_EXT_NAME = PREFIX_IS + "read-only";
    public static final String IS_INHERITED_EXT_NAME = PREFIX_IS + "inherited";
    public static final String IS_XML_ATTRIBUTE_EXT_NAME = PREFIX_IS + "xml-attribute";
    public static final String IS_XML_WRAPPED_EXT_NAME = PREFIX_IS + "xml-wrapped";
    public static final String IS_MULTIPART_EXT_NAME = PREFIX_IS + "multipart";
    public static final String IS_RESPONSE_BINARY_EXT_NAME = PREFIX_IS + "response-binary";
    public static final String IS_RESPONSE_FILE_EXT_NAME = PREFIX_IS + "response-file";
    public static final String IS_RESTFUL_INDEX_EXT_NAME = PREFIX_IS + "restful-index";
    public static final String IS_RESTFUL_SHOW_EXT_NAME = PREFIX_IS + "restful-show";
    public static final String IS_RESTFUL_CREATE_EXT_NAME = PREFIX_IS + "restful-create";
    public static final String IS_RESTFUL_UPDATE_EXT_NAME = PREFIX_IS + "restful-update";
    public static final String IS_RESTFUL_DESTROY_EXT_NAME = PREFIX_IS + "restful-destroy";
    public static final String IS_RESTFUL_EXT_NAME = PREFIX_IS + "restful";
    public static final String IS_DEPRECATED_EXT_NAME = PREFIX_IS + "deprecated";

    public static final String IS_FORM_EXT_NAME = PREFIX_IS + "form";

    public static final String IS_FORM_PARAM_EXT_NAME = PREFIX_IS + "form-param";
    public static final String IS_QUERY_PARAM_EXT_NAME = PREFIX_IS + "query-param";
    public static final String IS_PATH_PARAM_EXT_NAME = PREFIX_IS + "path-param";
    public static final String IS_HEADER_PARAM_EXT_NAME = PREFIX_IS + "header-param";
    public static final String IS_COOKIE_PARAM_EXT_NAME = PREFIX_IS + "cookie-param";
    public static final String IS_BODY_PARAM_EXT_NAME = PREFIX_IS + "body-param";
    public static final String IS_COLLECTION_FORMAT_MULTI_EXT_NAME = PREFIX_IS + "collection-format-multi";

    public static final String IS_BASIC_EXT_NAME = PREFIX_IS + "basic";
    public static final String IS_BEARER_EXT_NAME = PREFIX_IS + "bearer";
    public static final String IS_OAUTH_EXT_NAME = PREFIX_IS + "oauth";
    public static final String IS_API_KEY_EXT_NAME = PREFIX_IS + "api-key";
    public static final String IS_KEY_IN_QUERY_EXT_NAME = PREFIX_IS + "key-in-query";
    public static final String IS_KEY_IN_HEADER_EXT_NAME = PREFIX_IS + "key-in-header";
    public static final String IS_CODE_EXT_NAME = PREFIX_IS + "code";
    public static final String IS_PASSWORD_EXT_NAME = PREFIX_IS + "password";
    public static final String IS_APPLICATION_EXT_NAME = PREFIX_IS + "application";
    public static final String IS_IMPLICIT_EXT_NAME = PREFIX_IS + "implicit";

    public static final String IS_GET_METHOD_EXT_NAME = PREFIX_IS + "get-method";
    public static final String IS_POST_METHOD_EXT_NAME = PREFIX_IS + "post-method";
    public static final String IS_PUT_METHOD_EXT_NAME = PREFIX_IS + "put-method";
    public static final String IS_DELETE_METHOD_EXT_NAME = PREFIX_IS + "delete-method";
    public static final String IS_HEAD_METHOD_EXT_NAME = PREFIX_IS + "head-method";
    public static final String IS_TRACE_METHOD_EXT_NAME = PREFIX_IS + "trace-method";
    public static final String IS_PATCH_METHOD_EXT_NAME = PREFIX_IS + "patch-method";
    public static final String IS_OPTIONS_METHOD_EXT_NAME = PREFIX_IS + "options-method";

    public static final String HAS_MORE_EXT_NAME = PREFIX_HAS + "more";
    public static final String HAS_MORE_NON_READ_ONLY_EXT_NAME = PREFIX_HAS + "more-non-read-only";
    public static final String HAS_VALIDATION_EXT_NAME = PREFIX_HAS + "validation";
    public static final String HAS_AUTH_METHODS_EXT_NAME = PREFIX_HAS + "auth-methods";
    public static final String HAS_CONSUMES_EXT_NAME = PREFIX_HAS + "consumes";
    public static final String HAS_PRODUCES_EXT_NAME = PREFIX_HAS + "produces";
    public static final String HAS_PARAMS_EXT_NAME = PREFIX_HAS + "params";
    public static final String HAS_OPTIONAL_PARAMS_EXT_NAME = PREFIX_HAS + "optional-params";
    public static final String HAS_REQUIRED_PARAMS_EXT_NAME = PREFIX_HAS + "required-params";
    public static final String HAS_REFERENCE_EXT_NAME = PREFIX_HAS + "reference";
    public static final String HAS_HEADERS_EXT_NAME = PREFIX_HAS + "headers";

    public static final String MODEL_DOCS_OPTION = "--model-docs";
    public static final String API_DOCS_OPTION = "--api-docs";
    public static final String MODEL_TESTS_OPTION = "--model-tests";
    public static final String API_TESTS_OPTION = "--api-tests";
    public static final String USE_OAS2_OPTION = "--use-oas2";

    public static final String HANDLEBARS_TEMPLATE_ENGINE = "handlebars";
    public static final String MUSTACHE_TEMPLATE_ENGINE = "mustache";

}
