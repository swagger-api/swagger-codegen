package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.io.ClassPathTemplateLoader;
import com.github.jknack.handlebars.io.CompositeTemplateLoader;
import com.github.jknack.handlebars.io.FileTemplateLoader;
import com.github.jknack.handlebars.io.TemplateLoader;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConstants;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
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

    /**
     *     
     * @param originalTemplateFile the template file 
     * 
     * @return return the template 
     * 
     * @throws IOException
     */
    private Template getHandlebars(String originalTemplateFile) throws IOException
    {

        String templateFile = originalTemplateFile;
        templateFile = templateFile.replace(".mustache", StringUtils.EMPTY).replace("\\", "/");

        final TemplateLoader embeddedTemplateLoader = new ClassPathTemplateLoader("/" + config.embeddedTemplateDir(), ".mustache");
        final TemplateLoader templateLoader;
        
        // Get the custom template dir
        String templateDir = config.templateDir().replace(".mustache", StringUtils.EMPTY).replace("\\", "/");
        if (config.additionalProperties().get(CodegenConstants.TEMPLATE_DIR) != null && (new File(originalTemplateFile).exists() || new File(templateDir+originalTemplateFile).exists() ))
        {
            if (templateFile.startsWith(templateDir))
            {
                templateFile = StringUtils.replaceOnce(templateFile, templateDir, StringUtils.EMPTY);
            }
            // Using CompositeTemplateLoader to manage partial templates
            templateLoader = new CompositeTemplateLoader(new FileTemplateLoader(templateDir, ".mustache"),
                embeddedTemplateLoader);
        }
        // Get the embedded template dir
        else
        {
            if (templateFile.startsWith(config.embeddedTemplateDir()))
            {
                templateFile = StringUtils.replaceOnce(templateFile, config.embeddedTemplateDir(), StringUtils.EMPTY);
            }

            templateLoader = embeddedTemplateLoader;
        }

        final Handlebars handlebars = new Handlebars(templateLoader);
        handlebars.prettyPrint(true);
        config.addHandlebarHelpers(handlebars);
        return handlebars.compile(templateFile);
    }

}
