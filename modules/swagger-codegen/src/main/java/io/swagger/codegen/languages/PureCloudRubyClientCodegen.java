package io.swagger.codegen.languages;

import io.swagger.models.Operation;
import org.apache.commons.lang3.StringUtils;

public class PureCloudRubyClientCodegen extends RubyClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

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
     * Get the operation ID or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getVendorExtensions().containsKey(OPERATION_ID_PROPERTY_NAME)) {
            String operationId = operation.getVendorExtensions().get(OPERATION_ID_PROPERTY_NAME).toString();
            if (!StringUtils.isBlank(operationId)) {
                System.out.println("Using operation ID property " + OPERATION_ID_PROPERTY_NAME + " (" + operationId +  ") for path " + path);
                return operationId;
            }
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }
}
