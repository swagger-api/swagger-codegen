package io.swagger.codegen.v3;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CodegenContent implements VendorExtendable {

    private List<CodegenParameter> parameters = new ArrayList<CodegenParameter>();
    private Map<String, Object> vendorExtensions = new HashMap<>();

    public boolean getIsForm() {
        if (vendorExtensions.get(CodegenConstants.IS_FORM_EXT_NAME) == null) {
            return Boolean.FALSE;
        }
        return Boolean.parseBoolean(vendorExtensions.get(CodegenConstants.IS_FORM_EXT_NAME).toString());
    }

    public List<CodegenParameter> getParameters() {
        return parameters;
    }

    @Override
    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }
}
