package io.swagger.codegen.v3.service;

import java.io.Serializable;

public class RenderResponse implements Serializable {
    private static final long serialVersionUID = 1L;

    private String value;

    public RenderResponse value(String value) {
        this.value = value;
        return this;
    }

    public String getValue() {
        return value;
    }
    public void setValue(String value) {
        this.value = value;
    }

}
