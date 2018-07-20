package io.swagger.codegen.v3;

public class CodegenArgument {

    private String option;
    private String shortOption;
    private String description;
    private String type;
    private String value;
    private Boolean isArray = Boolean.FALSE;

    public String getOption() {
        return option;
    }

    public void setOption(String option) {
        this.option = option;
    }

    public CodegenArgument option(String option) {
        this.option = option;
        return this;
    }

    public String getShortOption() {
        return shortOption;
    }

    public void setShortOption(String shortOption) {
        this.shortOption = shortOption;
    }

    public CodegenArgument shortOption(String shortOption) {
        this.shortOption = shortOption;
        return this;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public CodegenArgument description(String description) {
        this.description = description;
        return this;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public CodegenArgument type(String type) {
        this.type = type;
        return this;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public CodegenArgument value(String value) {
        this.value = value;
        return this;
    }

    public Boolean getArray() {
        return isArray;
    }

    public void setArray(Boolean array) {
        isArray = array;
    }

    public CodegenArgument isArray(Boolean isArray) {
        this.isArray = isArray;
        return this;
    }
}
