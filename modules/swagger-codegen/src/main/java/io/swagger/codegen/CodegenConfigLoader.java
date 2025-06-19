package io.swagger.codegen;

import java.util.ServiceLoader;

import static java.util.ServiceLoader.load;

public class CodegenConfigLoader {
    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    public static CodegenConfig forName(String name) {
        ServiceLoader<CodegenConfig> loader = load(CodegenConfig.class);

        StringBuilder availableConfigs = new StringBuilder();
        CodegenConfig current = null;
        for (CodegenConfig config : loader) {

            if (config.getName().equals(name)) {
                if (current == null) {
                    current = config;
                } else if (config.isPrivileged() && !current.isPrivileged()) {
                    current = config;
                } else if (current.isPrivileged() && !config.isPrivileged()) {
                    // skip
                } else if (config.getPriority() > current.getPriority()) {
                    current = config;
                }
            }
            availableConfigs.append(config.getName()).append("\n");
        }
        if (current != null) {
            return current;
        }

        // else try to load directly
        try {
            return (CodegenConfig) Class.forName(name).newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Can't load config class with name ".concat(name) + " Available: " + availableConfigs.toString(), e);
        }
    }
}
