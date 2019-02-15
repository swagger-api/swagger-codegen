package io.swagger.codegen.utils;

import io.swagger.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE;
import io.swagger.codegen.DefaultCodegen;

public final class EnumPropertyNamingUtils {

    /** Apply the given {@link ENUM_PROPERTY_NAMING_TYPE} to get the correct capitalisation and underscore-usage for an enum name. */
    public static String applyEnumPropertyCapitalisation(String value, final ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming) {
        switch (enumPropertyNaming) {
            case original:
                return value;
            case camelCase:
                // NOTE: Removes hyphens and underscores
                return DefaultCodegen.camelize(value, true);
            case PascalCase:
                // NOTE: Removes hyphens and underscores
                return DefaultCodegen.titleCase(DefaultCodegen.camelize(value));
            case snake_case:
                // NOTE: Removes hyphens
                return DefaultCodegen.underscore(value);
            case UPPERCASE:
                return value.toUpperCase();
            default:
                return value;
        }
    }

    public static ENUM_PROPERTY_NAMING_TYPE parseEnumPropertyNaming(final String enumPropertyNamingType) {
        try {
            return ENUM_PROPERTY_NAMING_TYPE.valueOf(enumPropertyNamingType);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumPropertyNamingType + " is an invalid enum property naming option. Please choose from:");
            for (ENUM_PROPERTY_NAMING_TYPE t : ENUM_PROPERTY_NAMING_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
    }
}
