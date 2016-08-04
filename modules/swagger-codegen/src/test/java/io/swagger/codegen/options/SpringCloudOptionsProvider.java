package io.swagger.codegen.options;

public class SpringCloudOptionsProvider extends SpringOptionsProvider {

    @Override
    public String getLanguage() {
        return "spring-cloud";
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
