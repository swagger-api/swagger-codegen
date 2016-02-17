package io.swagger.codegen.languages;

import io.swagger.models.Operation;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PureCloudCSharpClientCodegen extends CSharpClientCodegen {

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudCSharpClientCodegen.class);

    public PureCloudCSharpClientCodegen() {
        super();

        // Use C# templates
        embeddedTemplateDir = templateDir = "csharp";

        // Prevents collision between System.Attribute and ININ.PureCloudApi.Model.Attribute
        typeMapping.put("Attribute", "ININ.PureCloudApi.Model.Attribute");
    }

    @Override
    public String getName() {
        return "purecloudcsharp";
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
}
