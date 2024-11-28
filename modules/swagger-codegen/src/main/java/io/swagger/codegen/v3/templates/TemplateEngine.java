package io.swagger.codegen.v3.templates;

import java.io.IOException;
import java.util.Map;

public interface TemplateEngine {

    String getRendered(String templateFile, Map<String, Object> templateData) throws IOException;

    String getName();
}
