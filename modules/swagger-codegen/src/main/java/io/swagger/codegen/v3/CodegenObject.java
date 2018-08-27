package io.swagger.codegen.v3;

import java.util.HashMap;
import java.util.Map;

public abstract class CodegenObject implements VendorExtendable {

    public Map<String, Object> vendorExtensions = new HashMap<>();

    @Override
    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }

    public void setVendorExtensions(Map<String, Object> vendorExtensions) {
        this.vendorExtensions = vendorExtensions;
    }

    public Boolean getHasHeaders() {
        return getBooleanValue(CodegenConstants.HAS_HEADERS_EXT_NAME);
    }

    public Boolean getIsString() {
        return getBooleanValue(CodegenConstants.IS_STRING_EXT_NAME);
    }

    public Boolean getIsNumeric() {
        return getBooleanValue(CodegenConstants.IS_NUMERIC_EXT_NAME);
    }

    public Boolean getIsInteger() {
        return getBooleanValue(CodegenConstants.IS_INTEGER_EXT_NAME);
    }

    public Boolean getIsLong() {
        return getBooleanValue(CodegenConstants.IS_LONG_EXT_NAME);
    }

    public Boolean getIsNumber() {
        return getBooleanValue(CodegenConstants.IS_NUMBER_EXT_NAME);
    }

    public Boolean getIsFloat() {
        return getBooleanValue(CodegenConstants.IS_FLOAT_EXT_NAME);
    }

    public Boolean getIsDouble() {
        return getBooleanValue(CodegenConstants.IS_DOUBLE_EXT_NAME);
    }

    public Boolean getIsByteArray() {
        return getBooleanValue(CodegenConstants.IS_BYTE_ARRAY_EXT_NAME);
    }

    public Boolean getIsBoolean() {
        return getBooleanValue(CodegenConstants.IS_BOOLEAN_EXT_NAME);
    }

    public Boolean getIsDate() {
        return getBooleanValue(CodegenConstants.IS_DATE_EXT_NAME);
    }

    public Boolean getIsDateTime() {
        return getBooleanValue(CodegenConstants.IS_DATE_TIME_EXT_NAME);
    }

    public Boolean getIsUuid() {
        return getBooleanValue(CodegenConstants.IS_UUID_EXT_NAME);
    }

    public Boolean getIsDefault() {
        return getBooleanValue(CodegenConstants.IS_DEFAULT_EXT_NAME);
    }

    public Boolean getIsMapContainer() {
        return getBooleanValue(CodegenConstants.IS_MAP_CONTAINER_EXT_NAME);
    }

    public Boolean getIsListContainer() {
        return getBooleanValue(CodegenConstants.IS_LIST_CONTAINER_EXT_NAME);
    }

    public Boolean getIsMultipart() {
        return getBooleanValue(CodegenConstants.IS_MULTIPART_EXT_NAME);
    }

    public Boolean getIsResponseBinary() {
        return getBooleanValue(CodegenConstants.IS_RESPONSE_BINARY_EXT_NAME);
    }

    public Boolean getIsResponseFile() {
        return getBooleanValue(CodegenConstants.IS_RESPONSE_FILE_EXT_NAME);
    }

    public Boolean getIsBinary() {
        return getBooleanValue(CodegenConstants.IS_BINARY_EXT_NAME);
    }

    public Boolean getIsFile() {
        return getBooleanValue(CodegenConstants.IS_FILE_EXT_NAME);
    }

    public Boolean getIsEnum() {
        return getBooleanValue(CodegenConstants.IS_ENUM_EXT_NAME);
    }

    public Boolean getIsArrayModel() {
        return getBooleanValue(CodegenConstants.IS_ARRAY_MODEL_EXT_NAME);
    }

    public Boolean getIsAlias() {
        return getBooleanValue(CodegenConstants.IS_ALIAS_EXT_NAME);
    }

    public Boolean getIsPrimitiveType() {
        return getBooleanValue(CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME);
    }

    public Boolean getIsContainer() {
        return getBooleanValue(CodegenConstants.IS_CONTAINER_EXT_NAME);
    }

    public Boolean getIsNotContainer() {
        return getBooleanValue(CodegenConstants.IS_NOT_CONTAINER_EXT_NAME);
    }

    public Boolean getIsReadOnly() {
        return getBooleanValue(CodegenConstants.IS_READ_ONLY_EXT_NAME);
    }

    public Boolean getIsCollectionFormatMulti() {
        return getBooleanValue(CodegenConstants.IS_COLLECTION_FORMAT_MULTI_EXT_NAME);
    }

    public Boolean getHasMore() {
        return getBooleanValue(CodegenConstants.HAS_MORE_EXT_NAME);
    }

    public Boolean getBooleanValue(Object key) {
        if (vendorExtensions.get(key) == null) {
            return Boolean.FALSE;
        }
        return Boolean.parseBoolean(vendorExtensions.get(key).toString());
    }
}
