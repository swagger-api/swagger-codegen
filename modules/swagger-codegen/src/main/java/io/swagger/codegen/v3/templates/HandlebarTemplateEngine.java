package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.io.TemplateLoader;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConstants;

import java.io.IOException;
import java.util.Map;

public class HandlebarTemplateEngine implements TemplateEngine {

    private CodegenConfig config;

    public HandlebarTemplateEngine(CodegenConfig config) {
        this.config = config;
    }

    @Override
    public String getRendered(String templateFile, Map<String, Object> templateData) throws IOException {
        final com.github.jknack.handlebars.Template hTemplate = getHandlebars(templateFile);
        return hTemplate.apply(templateData);
    }

    @Override
    public String getName() {
        return CodegenConstants.HANDLEBARS_TEMPLATE_ENGINE;
    }

    private com.github.jknack.handlebars.Template getHandlebars(String templateFile) throws IOException {
        templateFile = templateFile.replace("\\", "/");
        final String templateDir = config.templateDir().replace("\\", "/");
        final TemplateLoader templateLoader;
        String customTemplateDir = config.customTemplateDir() != null ? config.customTemplateDir().replace("\\", "/") : null;
        templateLoader = new CodegenTemplateLoader()
                .templateDir(templateDir)
                .customTemplateDir(customTemplateDir);
        final Handlebars handlebars = new Handlebars(templateLoader);
        handlebars.prettyPrint(true);
        config.addHandlebarHelpers(handlebars);
        return handlebars.compile(templateFile);
    }
}
