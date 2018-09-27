package io.swagger.codegen.v3.templates;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConstants;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Pattern;

public class MustacheTemplateEngine implements TemplateEngine {

    private static final Logger LOGGER = LoggerFactory.getLogger(MustacheTemplateEngine.class);

    private CodegenConfig config;

    public MustacheTemplateEngine(CodegenConfig config) {
        this.config = config;
    }

    @Override
    public String getRendered(String templateFile, Map<String, Object> templateData) throws IOException {
        String template = readTemplate(templateFile);

        Mustache.Compiler compiler = Mustache.compiler();
        compiler = config.processCompiler(compiler);
        Template tmpl = compiler
                .withLoader((name) -> {
                        final String fullTemplateFile = getFullTemplateFile(config, name + ".mustache");
                        return getTemplateReader(fullTemplateFile);
                })
                .defaultValue(StringUtils.EMPTY)
                .compile(template);

        return tmpl.execute(templateData);
    }

    @Override
    public String getName() {
        return CodegenConstants.MUSTACHE_TEMPLATE_ENGINE;
    }

    public String readTemplate(String name) {
        Reader reader = getTemplateReader(name);
        try {
            if (reader == null) {
                throw new RuntimeException("no file found");
            }
            final Scanner scanner = new Scanner(reader).useDelimiter("\\A");
            return scanner.hasNext() ? scanner.next() : "";
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    LOGGER.error(e.getMessage());
                }
            }
        }
        throw new RuntimeException("can't load template " + name);
    }

    public Reader getTemplateReader(String name) {
        try {
            InputStream is = this.getClass().getClassLoader().getResourceAsStream(getClassResourcePath(name));
            if (is == null) {
                is = new FileInputStream(new File(name));
            }
            return new InputStreamReader(is, "UTF-8");
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
        }
        throw new RuntimeException("can't load template " + name);
    }

    public String getFullTemplateFile(CodegenConfig config, String templateFile) {
        final String library = config.getLibrary();
        final String libTemplateFile = new StringBuilder(config.templateDir())
                .append(File.separator)
                .append("libraries")
                .append(File.separator)
                .append(templateFile)
                .toString();
        if (StringUtils.isNotEmpty(library)) {
            if (new File(libTemplateFile).exists()) {
                return libTemplateFile;
            }
        }
        final String template = config.templateDir() + File.separator + templateFile;
        if (new File(template).exists()) {
            return template;
        }
        if (StringUtils.isNotEmpty(library)) {
            if (this.getClass().getClassLoader().getResource(getClassResourcePath(libTemplateFile)) != null) {
                return libTemplateFile;
            }
        }
        return config.embeddedTemplateDir() + File.separator + templateFile;
    }

    public String getClassResourcePath(String name) {
        if (!"/".equals(File.separator)) {
            return name.replaceAll(Pattern.quote(File.separator), "/");
        }
        return name;
    }
}
