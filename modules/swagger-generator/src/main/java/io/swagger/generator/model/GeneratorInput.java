package io.swagger.generator.model;

import com.fasterxml.jackson.databind.JsonNode;

import io.swagger.annotations.ApiModelProperty;
import io.swagger.models.auth.AuthorizationValue;
import io.swagger.models.auth.SecuritySchemeDefinition;

import java.util.List;
import java.util.Map;

public class GeneratorInput {
    private JsonNode spec;
    private Map<String, String> options;
    private String swaggerUrl;
    private List<AuthorizationValue> swaggerAuth;
    private SecuritySchemeDefinition auth;

    @ApiModelProperty(dataType = "Object")
    public JsonNode getSpec() {
        return spec;
    }

    public void setSpec(JsonNode spec) {
        this.spec = spec;
    }

    public Map<String, String> getOptions() {
        return options;
    }

    public void setOptions(Map<String, String> options) {
        this.options = options;
    }

    @ApiModelProperty(example = "http://petstore.swagger.io/v2/swagger.json")
    public String getSwaggerUrl() {
        return swaggerUrl;
    }

    public void setSwaggerUrl(String url) {
        this.swaggerUrl = url;
    }

    public List<AuthorizationValue> getSwaggerAuth() {
        return swaggerAuth;
    }

    public void setSwaggerAuth(List<AuthorizationValue> swaggerAuth) {
        this.swaggerAuth = swaggerAuth;
    }

    public SecuritySchemeDefinition getSecurityDefinition() {
        return auth;
    }

    public void setSecurityDefinition(SecuritySchemeDefinition auth) {
        this.auth = auth;
    }
}