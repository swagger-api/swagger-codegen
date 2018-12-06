package io.swagger.codegen.v3.service;

import java.io.Serializable;

public class GenerationRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    private String lang;
    private Object spec = null;
    private String specURL = null;
    private Options options = new Options();
    private Type type;
    private CodegenVersion codegenVersion = CodegenVersion.V3;

    public enum Type {
        CLIENT("client"), SERVER("server"), DOCUMENTATION("documentation"), CONFIG("config");

        private String name;

        Type(String name) {
            this.name = name;
        }

        public String getTypeName() {
            return name;
        }

        public static Type fromValue(String name) {
            for (Type t: Type.values()) {
                if (name.equals(t.name)) {
                    return t;
                }
            }
            return null;
        }
    }

    public enum CodegenVersion {
        V2, V3;
    }

    public GenerationRequest lang(String lang) {
        this.lang = lang;
        return this;
    }

    public String getLang() {
        return lang;
    }
    public void setLang(String lang) {
        this.lang = lang;
    }


    public GenerationRequest spec(Object spec) {
        this.spec = spec;
        return this;
    }

    public GenerationRequest specURL(String specURL) {
        this.specURL = specURL;
        return this;
    }

    public Object getSpec() {
        return spec;
    }

    public void setSpec(Object spec) {
        this.spec = spec;
    }

    public String getSpecURL() {
        return specURL;
    }

    public void setSpecURL(String specURL) {
        this.specURL = specURL;
    }

    public GenerationRequest options(Options options) {
        this.options = options;
        return this;
    }

    public Options getOptions() {
        return options;
    }

    public void setOptions(Options options) {
        this.options = options;
    }

    public GenerationRequest type(Type type) {
        this.type = type;
        return this;
    }
    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public GenerationRequest codegenVersion(CodegenVersion codegenVersion) {
        this.codegenVersion = codegenVersion;
        return this;
    }
    public CodegenVersion getCodegenVersion() {
        return codegenVersion;
    }

    public void setCodegenVersion(CodegenVersion codegenVersion) {
        this.codegenVersion = codegenVersion;
    }

}
