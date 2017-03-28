package io.swagger.codegen.languages;

import io.swagger.models.Operation;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class PureCloudPythonClientCodegen extends PythonClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudPythonClientCodegen.class);

    public PureCloudPythonClientCodegen() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "python";

        reservedWords.add("property");
    }


    @Override
    public String getName() {
        return "purecloudpython";
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
                return operationId;
            }
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }

    @Override
    public void processOpts() {
        super.processOpts();
    }

    @Override
    public String escapeReservedWord(String name) {
        return "pc" + capitalizeFirstLetter(name);
    }

    private String capitalizeFirstLetter(String s) {
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }
}
