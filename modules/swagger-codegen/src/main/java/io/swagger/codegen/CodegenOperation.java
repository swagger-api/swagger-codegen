package io.swagger.codegen;

import io.swagger.models.ExternalDocs;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CodegenOperation {
    public final List<CodegenProperty> responseHeaders = new ArrayList<CodegenProperty>();
    public Boolean hasAuthMethods, hasConsumes, hasProduces, hasParams, hasOptionalParams,
            returnTypeIsPrimitive, returnSimpleType, subresourceOperation, isMapContainer,
            isListContainer, isMultipart, hasMore = Boolean.TRUE,
            isResponseBinary = Boolean.FALSE, hasReference = Boolean.FALSE,
            isRestfulIndex, isRestfulShow, isRestfulCreate, isRestfulUpdate, isRestfulDestroy,
            isRestful;
    public String path, operationId, returnType, httpMethod, returnBaseType,
            returnContainer, summary, unescapedNotes, notes, baseName, defaultResponse, discriminator;
    public List<Map<String, String>> consumes, produces;
    public CodegenParameter bodyParam;
    public List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();
    public List<CodegenSecurity> authMethods;
    public List<String> tags;
    public List<CodegenResponse> responses = new ArrayList<CodegenResponse>();
    public Set<String> imports = new HashSet<String>();
    public List<Map<String, String>> examples;
    public ExternalDocs externalDocs;
    public Map<String, Object> vendorExtensions;
    public String nickname; // legacy support
    public String operationIdLowerCase; // for mardown documentation

    /**
     * Check if there's at least one parameter
     *
     * @return true if parameter exists, false otherwise
     */
    private static boolean nonempty(List<?> params) {
        return params != null && params.size() > 0;
    }
    
    /**
     * Check if there's at least one body parameter
     *
     * @return true if body parameter exists, false otherwise
     */
    public boolean getHasBodyParam() {
        return nonempty(bodyParams);
    }

    /**
     * Check if there's at least one query parameter
     *
     * @return true if query parameter exists, false otherwise
     */
    public boolean getHasQueryParams() {
        return nonempty(queryParams);
    }

    /**
     * Check if there's at least one header parameter
     *
     * @return true if header parameter exists, false otherwise
     */
    public boolean getHasHeaderParams() {
        return nonempty(headerParams);
    }

    /**
     * Check if there's at least one path parameter
     *
     * @return true if path parameter exists, false otherwise
     */
    public boolean getHasPathParams() {
        return nonempty(pathParams);
    }

    /**
     * Check if there's at least one form parameter
     *
     * @return true if any form parameter exists, false otherwise
     */
    public boolean getHasFormParams() {
        return nonempty(formParams);
    }

    /**
     * Check if there's at least one example parameter
     *
     * @return true if examples parameter exists, false otherwise
     */
    public boolean getHasExamples() {
        return nonempty(examples);
    }

    /**
     * Check if act as Restful index method
     *
     * @return true if act as Restful index method, false otherwise
     */
    public boolean isRestfulIndex() {
        return "GET".equals(httpMethod) && "".equals(pathWithoutBaseName());
    }

    /**
     * Check if act as Restful show method
     *
     * @return true if act as Restful show method, false otherwise
     */
    public boolean isRestfulShow() {
        return "GET".equals(httpMethod) && isMemberPath();
    }

    /**
     * Check if act as Restful create method
     *
     * @return true if act as Restful create method, false otherwise
     */
    public boolean isRestfulCreate() {
        return "POST".equals(httpMethod) && "".equals(pathWithoutBaseName());
    }

    /**
     * Check if act as Restful update method
     *
     * @return true if act as Restful update method, false otherwise
     */
    public boolean isRestfulUpdate() {
        return Arrays.asList("PUT", "PATCH").contains(httpMethod) && isMemberPath();
    }

    /**
     * Check if act as Restful destroy method
     *
     * @return true if act as Restful destroy method, false otherwise
     */
    public boolean isRestfulDestroy() {
        return "DELETE".equals(httpMethod) && isMemberPath();
    }

    /**
     * Check if Restful-style
     *
     * @return true if Restful-style, false otherwise
     */
    public boolean isRestful() {
        return isRestfulIndex() || isRestfulShow() || isRestfulCreate() || isRestfulUpdate() || isRestfulDestroy();
    }

    /**
     * Get the substring except baseName from path
     *
     * @return the substring
     */
    private String pathWithoutBaseName() {
        return baseName != null ? path.replace("/" + baseName.toLowerCase(), "") : path;
    }

    /**
     * Check if the path match format /xxx/:id
     *
     * @return true if path act as member
     */
    private boolean isMemberPath() {
        if (pathParams.size() != 1) return false;

        String id = pathParams.get(0).baseName;
        return ("/{" + id + "}").equals(pathWithoutBaseName());
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", baseName, path);
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
