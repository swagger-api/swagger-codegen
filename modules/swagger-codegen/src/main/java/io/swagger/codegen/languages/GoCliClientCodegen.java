package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class GoCliClientCodegen extends PureCloudGoClientCodegen {
    protected Logger LOGGER = LoggerFactory.getLogger(GoCliClientCodegen.class);

    public GoCliClientCodegen() {
        super();

        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");

        outputFolder = "generated-code/go";

        embeddedTemplateDir = templateDir = "go";
    }

    @Override
    public String getName() {
        return "clisdkclient";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Wipe out all the templates so we can start with a clean slate
        this.apiTemplateFiles.clear();
        this.apiDocTemplateFiles.clear();
        this.apiTestTemplateFiles.clear();
        this.modelTemplateFiles.clear();
        this.modelDocTemplateFiles.clear();
        this.modelTestTemplateFiles.clear();
        this.operationTemplateFiles.clear();
        this.supportingFiles.clear();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("addImports", "{{=it.addImports}}");
        additionalProperties.put("addCommands", "{{=it.addCommands}}");

        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("restclient.mustache", "/gc/restclient", "restclient.go"));
        supportingFiles.add(new SupportingFile("root.mustache", "/gc/cmd", "root.go"));
        apiTemplateFiles.put("api.mustache", ".go");
    }

    public String apiFileFolder() {
        return (outputFolder + "/gc/cmd").replace('/', File.separatorChar);
    }

    @Override
    public String toApiFilename(String name) {
        name = name.toLowerCase();
        return name + File.separatorChar + name;
    }

    private int firstIndexOfCapital(String str) {
        for (int i = 0; i < str.length(); i++) {
            if (Character.isUpperCase(str.charAt(i))) {
                return i;
            }
        }
        return -1;
    }

    @Override
    public String toOperationId(String operationId) {
        final String SWAGGER_OVERRIDE = "SWAGGER_OVERRIDE_";
        if (operationId.contains(SWAGGER_OVERRIDE)) {
            return operationId.replace(SWAGGER_OVERRIDE, "");
        }

        // OperationIds look like getRoutingSkills or deleteRoutingSkill etc, change it to gets or delete in this step
        boolean isPlural = operationId.endsWith("s");
        operationId = operationId.substring(0, firstIndexOfCapital(operationId));
        operationId += isPlural ? "s" : "";

        // post -> create
        // patch or put -> update (will cause a clash if a resource has both, this would have to be resolved by overriding one or both operationIds)
        // get -> get (no change)
        // gets -> list
        operationId = operationId
                .replaceAll("^post", "create")
                .replaceAll("^patch|^put", "update");
        if (operationId.startsWith("get") && operationId.endsWith("s"))
            operationId = operationId.replaceAll("^get", "list");
        operationId = operationId.replaceAll("s*$", "");

        return operationId;
    }

    @Override
    /**
     * Get the operation ID or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        return operation.getOperationId();
    }

    /**
     * Remove characters not suitable for variable or method name from the input and camelize it
     *
     * @param name string to be camelize
     * @return camelized string
     */
    @SuppressWarnings("static-method")
    public String removeNonNameElementToCamelCase(String name) {
        // Change to no-op
        return name;
    }

    @Override
    /**
     * Add operation to group
     *
     * @param tag name of the tag
     * @param resourcePath path of the resource
     * @param operation Swagger Operation object
     * @param co Codegen Operation object
     * @param operations map of Codegen operations
     */
    @SuppressWarnings("static-method")
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(tag);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(tag, opList);
        }

        co.operationIdLowerCase = co.operationId.toLowerCase();
        opList.add(co);
        co.baseName = tag;
    }

    @Override
    public String toApiVarName(String name) {
        return name.toLowerCase();
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        if (parameter.description != null) {
            parameter.description = parameter.description
                    .replace("\\\"", "")
                    .replace("'", "`");
        }
    }

    @Override
    public String sanitizeTag(String tag) {
        // remove spaces and make strong case
        String[] parts = tag.split(" ");
        StringBuilder buf = new StringBuilder();
        for (String part : parts) {
            if (StringUtils.isNotEmpty(part)) {
                buf.append(StringUtils.capitalize(part));
            }
        }
        return buf.toString().replaceAll("[^a-zA-Z_ ]", "");
    }
}
