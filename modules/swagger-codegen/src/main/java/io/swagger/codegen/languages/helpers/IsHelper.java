package io.swagger.codegen.languages.helpers;

import static io.swagger.codegen.VendorExtendable.PREFIX_IS;

/**
 * new version of this class can be found on: https://github.com/swagger-api/swagger-codegen-generators
 * @deprecated use <code>io.swagger.codegen.handlebars.helpers.IsHelper</code> instead.
 */
@Deprecated
public class IsHelper extends ExtensionHelper {

    public static final String NAME = "is";

    @Override
    public String getPreffix() {
        return PREFIX_IS;
    }
}
