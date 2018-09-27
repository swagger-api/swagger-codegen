package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.io.ClassPathTemplateLoader;
import com.github.jknack.handlebars.io.FileTemplateLoader;
import com.github.jknack.handlebars.io.TemplateLoader;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConstants;
import org.apache.commons.lang3.StringUtils;

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
        templateFile = templateFile.replace(".mustache", StringUtils.EMPTY).replace("\\", "/");
        String templateDir = config.templateDir().replace(".mustache", StringUtils.EMPTY).replace("\\", "/");
        if (templateFile.startsWith(templateDir)) {
            templateFile = StringUtils.replaceOnce(templateFile, templateDir, StringUtils.EMPTY);
        }
        TemplateLoader templateLoader = null;
        if (config.additionalProperties().get(CodegenConstants.TEMPLATE_DIR) != null) {
            templateLoader = new FileTemplateLoader(templateDir, ".mustache");
        } else {
            templateLoader = new ClassPathTemplateLoader("/" + templateDir, ".mustache");
        }
        final Handlebars handlebars = new Handlebars(templateLoader);
        handlebars.prettyPrint(true);
        config.addHandlebarHelpers(handlebars);
        return handlebars.compile(templateFile);
    }
}
