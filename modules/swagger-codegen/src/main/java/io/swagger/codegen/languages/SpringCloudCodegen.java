package io.swagger.codegen.languages;

import io.swagger.codegen.*;

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
