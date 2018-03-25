package io.swagger.codegen.languages.helpers;

import static io.swagger.codegen.VendorExtendable.PREFIX_HAS;

/**
 * new version of this class can be found on: https://github.com/swagger-api/swagger-codegen-generators
 * @deprecated use <code>io.swagger.codegen.handlebars.helpers.HasHelper</code> instead.
 */
@Deprecated
public class HasHelper extends ExtensionHelper {

    public static final String NAME = "has";

    @Override
    public String getPreffix() {
        return PREFIX_HAS;
    }
}
