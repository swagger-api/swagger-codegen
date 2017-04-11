package io.swagger.codegen;

import static com.google.common.base.Objects.equal;

import java.io.IOException;
import java.util.Map;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.io.FileTemplateLoader;
import com.github.jknack.handlebars.io.TemplateLoader;

/**
 * @author Tobias Gmünder
 */
public class HandlebarsTemplateRenderer {

    private final CodegenConfig config;

    private final Handlebars handlebars;

    /**
     * Creates a new instance with the given {@code config}.
     *
     * @param config  the {@link CodegenConfig}.
     */
    public HandlebarsTemplateRenderer(CodegenConfig config) {
        this.config = config;
        final TemplateLoader templateLoader = new FileTemplateLoader(config.templateDir());
        templateLoader.setSuffix(".mustache");
        this.handlebars = new Handlebars(templateLoader);

        handlebars.registerHelper("equals", new Helper<Object>() {
            @Override
            public Object apply(Object context, Options options) throws IOException {
                final Object obj = options.param(0);
                return equal(context, obj) ? options.fn() : options.inverse();
            }
        });
    }

    public String renderHandlebarTemplate(String templateName, Map<String, Object> templateData) throws IOException {
        final com.github.jknack.handlebars.Template hTemplate = handlebars.compile(templateName.replaceFirst(".mustache", ""));
        return hTemplate.apply(templateData);
    }

    /**
     * @return the {@link Handlebars} instance used for template rendering. Use this method to register custom helpers.
     */
    public Handlebars getHandlebars() {
        return this.handlebars;
    }

}
