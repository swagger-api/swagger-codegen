package io.swagger.codegen.v3.service;

import java.io.Serializable;

public class RenderRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    private String template;
    private String context;

    public RenderRequest template(String template) {
        this.template = template;
        return this;
    }

    public String getTemplate() {
        return template;
    }
    public void setTemplate(String template) {
        this.template = template;
    }

    public RenderRequest context(String context) {
        this.context = context;
        return this;
    }

    public String getContext() {
        return context;
    }
    public void setContext(String context) {
        this.context = context;
    }

}
