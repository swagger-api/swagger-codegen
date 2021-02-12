package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WebMessagingJavaClientCodegen extends PureCloudJavaClientCodegen {
    protected Logger LOGGER = LoggerFactory.getLogger(WebMessagingJavaClientCodegen.class);

    public WebMessagingJavaClientCodegen() {
        super();
    }

    @Override
    public String getName() {
        return "webmessagingjava";
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

        modelTemplateFiles.put("model.mustache", ".java");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiTemplateFiles.put("api.mustache", ".java");
        operationTemplateFiles.put("requestBuilder.mustache", ".java");

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        supportingFiles.add(new SupportingFile("WebMessagingClient.mustache", invokerFolder, "WebMessagingClient.java"));
        supportingFiles.add(new SupportingFile("WebMessagingException.mustache", invokerFolder, "WebMessagingException.java"));
        supportingFiles.add(new SupportingFile("GenesysCloudRegionWebSocketHosts.mustache", invokerFolder, "GenesysCloudRegionWebSocketHosts.java"));
        supportingFiles.add(new SupportingFile("ApiDateFormat.mustache", invokerFolder, "ApiDateFormat.java"));
        supportingFiles.add(new SupportingFile("LocalDateDeserializer.mustache", invokerFolder, "LocalDateDeserializer.java"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
        supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));

        final String authFolder = (sourceFolder + '/' + invokerPackage + ".auth").replace(".", "/");
        supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
        supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));

        writeOptional(outputFolder, new SupportingFile("ApiClient.mustache", invokerFolder, "ApiClient.java"));
    }

    @Override
    public String getterAndSetterCapitalize(String name) {
        String sanitizedName = name;

        // Special handling to avoid a method conflict named "getClass"
        if ("class".equalsIgnoreCase(name)) {
            sanitizedName = "classProperty";
        }

        return super.getterAndSetterCapitalize(sanitizedName);
    }
}
