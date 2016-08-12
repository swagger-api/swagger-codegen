package io.swagger.codegen;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.List;
import java.util.Map;

public class CodegenProperty implements Cloneable {
    public String baseName, complexType, getter, setter, description, datatype, datatypeWithEnum,
            dataFormat, name, min, max, defaultValue, defaultValueWithParam, baseType, containerType;

    public String unescapedDescription;

    /**
     * maxLength validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.1
     */
    public Integer maxLength;
    /**
     * minLength validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.2
     */
    public Integer minLength;
    /**
     * pattern validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.3
     */
    public String pattern;
    /**
     * A free-form property to include an example of an instance for this schema.
     */
    public String example;

    public String jsonSchema;
    public Double minimum;
    public Double maximum;
    public Boolean exclusiveMinimum;
    public Boolean exclusiveMaximum;
    public Boolean hasMore, required, secondaryParam;
    public Boolean hasMoreNonReadOnly; // for model constructor, true if next properyt is not readonly
    public Boolean isPrimitiveType, isContainer, isNotContainer;
    public Boolean isString, isInteger, isLong, isFloat, isDouble, isByteArray, isBinary, isBoolean, isDate, isDateTime;
    public Boolean isListContainer, isMapContainer;
    public boolean isEnum;
    public Boolean isReadOnly = false;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public Map<String, Object> vendorExtensions;
    public Boolean hasValidation; // true if pattern, maximum, etc are set (only used in the mustache template)
    public Boolean isInherited;
    public String nameInCamelCase; // property name in camel case
    // enum name based on the property name, usually use as a prefix (e.g. VAR_NAME) for enum name (e.g. VAR_NAME_VALUE1)
    public String enumName; 

    @Override
    public String toString() {
        return String.format("%s(%s)", baseName, datatype);
    }


    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    @Override
    public CodegenProperty clone() {
        try {
            return (CodegenProperty) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
    }
}
