package io.swagger.codegen.languages.helpers;

import io.swagger.codegen.VendorExtendable;

/**
 * new version of this class can be found on: https://github.com/swagger-api/swagger-codegen-generators
 * @deprecated use <code>io.swagger.codegen.handlebars.helpers.IsNotHelper</code> instead.
 */
@Deprecated
public class IsNotHelper extends NoneExtensionHelper {

    public static final String NAME = "isNot";


    @Override
    public String getPreffix() {
        return VendorExtendable.PREFIX_IS;
    }
}
