package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
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

        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("restclient.mustache", "/src/restclient", "restclient.go"));
        apiTemplateFiles.put("api.mustache", ".go");
    }

    public String apiFileFolder() {
        return (outputFolder + "/src/cmd").replace('/', File.separatorChar);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        String folder = name;

        return toCustomApiName((folder + File.separatorChar + name));
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }
        operationId = operationId
                .toLowerCase()
                .replaceAll("outboundcampaign|authorizationdivision|telephonyprovidersedge|group|location|sphone|routingqueue|ssite|routingskill|station|usagequery|listexecutionid|user", "")
                .replaceAll("^post", "create")
                .replaceAll("^patch|^put", "update");
        if (operationId.startsWith("get") && operationId.endsWith("s"))
            operationId = operationId.replaceAll("^get", "list");

        operationId = operationId.replaceAll("s*$", "")
                .replaceAll("listexecutionidresult", "results");

        return operationId;
    }

    @Override
    public String toApiVarName(String name) {
        return toCustomApiName(name);
    }

    private String toCustomApiName(String name) {
        // Renaming APIs as necessary to create more user friendly names for the CLI interface
        return name
                .toLowerCase()
                .replaceAll("outbound", "campaigns")
                .replaceAll("authorization", "divisions")
                .replaceAll("telephonyprovidersedge", "edges")
                .replaceAll("routing", "queues");
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        if (parameter.description != null) {
            parameter.description = parameter.description
                    .replace("\\\"", "");
        }
    }
}
