package io.swagger.codegen.v3;

import io.swagger.v3.oas.models.security.Scopes;

import java.util.HashMap;
import java.util.Map;

public class CodegenSecurity extends CodegenObject {
    public String name;
    public String type;
    // ApiKey specific
    public String keyParamName;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public Scopes scopes;

    @Override
    public String toString() {
        return String.format("%s(%s)", name, type);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenSecurity that = (CodegenSecurity) o;

        if (name != null ? !name.equals(that.name) : that.name != null)
            return false;
        if (type != null ? !type.equals(that.type) : that.type != null)
            return false;
        if (keyParamName != null ? !keyParamName.equals(that.keyParamName) : that.keyParamName != null)
            return false;
        if (flow != null ? !flow.equals(that.flow) : that.flow != null)
            return false;
        if (authorizationUrl != null ? !authorizationUrl.equals(that.authorizationUrl) : that.authorizationUrl != null)
            return false;
        if (tokenUrl != null ? !tokenUrl.equals(that.tokenUrl) : that.tokenUrl != null)
            return false;
        return scopes != null ? scopes.equals(that.scopes) : that.scopes == null;

    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (keyParamName != null ? keyParamName.hashCode() : 0);
        result = 31 * result + (flow != null ? flow.hashCode() : 0);
        result = 31 * result + (authorizationUrl != null ? authorizationUrl.hashCode() : 0);
        result = 31 * result + (tokenUrl != null ? tokenUrl.hashCode() : 0);
        result = 31 * result + (scopes != null ? scopes.hashCode() : 0);
        return result;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public String getKeyParamName() {
        return keyParamName;
    }

    public String getFlow() {
        return flow;
    }

    public String getAuthorizationUrl() {
        return authorizationUrl;
    }

    public String getTokenUrl() {
        return tokenUrl;
    }

    public Scopes getScopes() {
        return scopes;
    }

    public Boolean getIsBasic() {
        return getBooleanValue(CodegenConstants.IS_BASIC_EXT_NAME);
    }

    public Boolean getIsOAuth() {
        return getBooleanValue(CodegenConstants.IS_OAUTH_EXT_NAME);
    }

    public Boolean getIsApiKey() {
        return getBooleanValue(CodegenConstants.IS_API_KEY_EXT_NAME);
    }

    public Boolean getIsKeyInQuery() {
        return getBooleanValue(CodegenConstants.IS_KEY_IN_QUERY_EXT_NAME);
    }

    public Boolean getIsKeyInHeader() {
        return getBooleanValue(CodegenConstants.IS_KEY_IN_HEADER_EXT_NAME);
    }

    public Boolean getIsCode() {
        return getBooleanValue(CodegenConstants.IS_CODE_EXT_NAME);
    }

    public Boolean getIsPassword() {
        return getBooleanValue(CodegenConstants.IS_PASSWORD_EXT_NAME);
    }

    public Boolean getIsApplication() {
        return getBooleanValue(CodegenConstants.IS_APPLICATION_EXT_NAME);
    }

    public Boolean getIsImplicit() {
        return getBooleanValue(CodegenConstants.IS_IMPLICIT_EXT_NAME);
    }
}
