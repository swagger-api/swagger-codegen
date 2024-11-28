package io.swagger.codegen.v3.config;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class Config {
    private Map<String, String> options;

    public Config() {
        this.options = new HashMap<String, String>();
    }

    public Config(Map<String, String> properties) {
        this.options = properties;
    }

    public Map<String, String> getOptions() {
        return Collections.unmodifiableMap(options);
    }

    public boolean hasOption(String opt) {
        return options.containsKey(opt);
    }

    public String getOption(String opt) {
        return options.get(opt);
    }

    public void setOption(String opt, String value) {
        options.put(opt, value);
    }
}
