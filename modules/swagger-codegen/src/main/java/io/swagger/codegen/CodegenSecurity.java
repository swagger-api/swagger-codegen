package io.swagger.codegen;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.List;
import java.util.Map;

public class CodegenSecurity {
    public String name;
    public String type;
    public Boolean hasMore, isBasic, isOAuth, isApiKey;
    // ApiKey specific
    public String keyParamName;
    public Boolean isKeyInQuery, isKeyInHeader;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public List<Map<String, Object>> scopes;
    public Boolean isCode, isPassword, isApplication, isImplicit;

    @Override
    public String toString() {
        return String.format("%s(%s)", name, type);
    }

    @Override
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);

    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }
}
