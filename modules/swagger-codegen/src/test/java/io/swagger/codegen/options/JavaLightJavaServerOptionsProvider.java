package io.swagger.codegen.options;

/**
 * Created by steve on 18/09/16.
 */
public class JavaLightJavaServerOptionsProvider extends JavaOptionsProvider {
    @Override
    public String getLanguage() {
        return "light-java";
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
