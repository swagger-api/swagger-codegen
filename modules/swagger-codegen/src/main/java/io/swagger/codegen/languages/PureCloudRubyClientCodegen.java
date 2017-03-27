package io.swagger.codegen.languages;

import io.swagger.models.Operation;
import org.apache.commons.lang3.StringUtils;

public class PureCloudRubyClientCodegen extends RubyClientCodegen {

    public PureCloudRubyClientCodegen() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "ruby";
    }

    @Override
    public String getName() {
        return "purecloudruby";
    }

    @Override
    /**
     * Get the value of x-purecloud-method-name, or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getVendorExtensions().containsKey("x-purecloud-method-name")) {
            String ininMethodName = operation.getVendorExtensions().get("x-purecloud-method-name").toString();
            if (!StringUtils.isBlank(ininMethodName)) return ininMethodName;
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }
}
