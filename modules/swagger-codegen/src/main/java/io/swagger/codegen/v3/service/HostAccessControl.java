package io.swagger.codegen.v3.service;

public class HostAccessControl {
    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public boolean isRegex() {
        return regex;
    }

    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    public boolean isEndsWith() {
        return endsWith;
    }

    public void setEndsWith(boolean endsWith) {
        this.endsWith = endsWith;
    }

    private String host;
    private boolean regex;
    private boolean endsWith;
}
