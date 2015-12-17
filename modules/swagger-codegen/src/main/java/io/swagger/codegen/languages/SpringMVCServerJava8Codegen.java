package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;

/**
 * Extends spring-mvc code-gen, uses Java 8 default interface default and provide async callback.
 *
 * This is mainly for Maven code-gen plugin use-case. When Swagger spec on existing project changed
 * there is no need to manually copy/paste the new functions from the generated client. This will
 * provide a default (empty) implementation to existing impl and user just need to override it.
 *
 * Because it is an interface, an actual impl will be needed to actuate a service endpoint.
 */
public class SpringMVCServerJava8Codegen extends SpringMVCServerCodegen implements CodegenConfig {

    public SpringMVCServerJava8Codegen() {
        super();
    }

    @Override
    protected String getTemplateFileName() {
        return "api-j8.mustache";
    }

    @Override
    public String getName() {
        return "spring-mvc-j8";
    }
}
