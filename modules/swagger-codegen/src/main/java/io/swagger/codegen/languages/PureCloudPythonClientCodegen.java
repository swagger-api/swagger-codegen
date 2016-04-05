package io.swagger.codegen.languages;

import io.swagger.models.Operation;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class PureCloudPythonClientCodegen extends PythonClientCodegen {

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudCSharpClientCodegen.class);

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
    public void processOpts() {
        super.processOpts();
    }

    @Override
    public String escapeReservedWord(String name) {
        return "pc" + capitalizeFirstLetter(name);
    }

    @Override
    /**
     * Get the value of x-inin-method-name, or use default C# behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getVendorExtensions().containsKey("x-inin-method-name")) {
            String ininMethodName = operation.getVendorExtensions().get("x-inin-method-name").toString();
            if (!StringUtils.isBlank(ininMethodName)) return ininMethodName;
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }

    private String capitalizeFirstLetter(String s) {
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }
}
