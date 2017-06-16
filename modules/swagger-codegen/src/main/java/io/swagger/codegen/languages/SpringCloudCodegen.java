package io.swagger.codegen.languages;

import io.swagger.codegen.*;

/**
 * This is a temporary workaround, so spring cloud can be listed by
 *
 * http://generator.swagger.io/api/gen/clients
 *
 * as a client and eventually Spring cloud can be displayed by the swagger editor' generate client menu.
 */
public class SpringCloudCodegen extends SpringCodegen {

    public SpringCloudCodegen() {
        super();
        setLibrary(SPRING_CLOUD_LIBRARY);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "spring-cloud";
    }

    @Override
    public String getHelp() {
        return supportedLibraries.get(SPRING_CLOUD_LIBRARY);
    }

}
