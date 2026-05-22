package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.io.TemplateLoader;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConstants;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

// Caches both the Handlebars runtime and compiled templates for the lifetime of one generation:
//   * one Handlebars instance, built lazily on first use - avoids rebuilding the loader,
//     re-registering helpers, and re-instantiating the ANTLR runtime on every render;
//   * a per-templateFile map of compiled Templates so entry templates (controller, model, ...)
//     are parsed once even though they are rendered hundreds of times;
//   * IndentAwareTemplateCache so partials referenced via {{> ... }} are parsed once per
//     unique (filename, applied-indent) pair - this is the dominant share of parse time.
//     The stock ConcurrentMapTemplateCache from jknack collides distinct include sites of
//     the same partial because its TemplateSource wrapper's equals/hashCode ignore the
//     applied indent (see IndentAwareTemplateCache for the full rationale).
// Before this change, getRendered re-created Handlebars and re-parsed the template (and all of
// its partials) on every invocation, which made the Handlebars ANTLR lexer ~65% of CPU on
// realistic specs.
public class HandlebarTemplateEngine implements TemplateEngine {

    private final CodegenConfig config;
    private final Map<String, com.github.jknack.handlebars.Template> compiledTemplates = new ConcurrentHashMap<>();
    private volatile Handlebars handlebars;

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
        final String key = templateFile.replace("\\", "/");
        com.github.jknack.handlebars.Template cached = compiledTemplates.get(key);
        if (cached != null) {
            return cached;
        }
        final com.github.jknack.handlebars.Template compiled = handlebars().compile(key);
        compiledTemplates.put(key, compiled);
        return compiled;
    }

    private Handlebars handlebars() {
        Handlebars local = handlebars;
        if (local != null) {
            return local;
        }
        synchronized (this) {
            if (handlebars == null) {
                final String templateDir = config.templateDir().replace("\\", "/");
                final String customTemplateDir = config.customTemplateDir() != null
                    ? config.customTemplateDir().replace("\\", "/")
                    : null;
                final TemplateLoader templateLoader = new CodegenTemplateLoader()
                    .templateDir(templateDir)
                    .customTemplateDir(customTemplateDir);
                final Handlebars hb = new Handlebars(templateLoader);
                hb.prettyPrint(true);
                hb.with(new IndentAwareTemplateCache());
                config.addHandlebarHelpers(hb);
                handlebars = hb;
            }
            return handlebars;
        }
    }
}
