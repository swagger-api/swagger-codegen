package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.io.FileTemplateLoader;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.io.IOException;
import java.net.URL;

public class CodegenTemplateLoader extends FileTemplateLoader {
    private String templateDir;
    private String customTemplateDir;

    public CodegenTemplateLoader(String templateDir, String suffix) {
        super(templateDir, suffix);
        this.templateDir = templateDir;
    }

    public URL getResource(String location) throws IOException {
        if (this.customTemplateDir == null) {
            return this.getClass().getResource(location);
        }
        String templateFile = resolveTemplateFile(this.templateDir, location);
        templateFile = resolveTemplateFile(this.customTemplateDir, templateFile);
        final File file = new File(this.customTemplateDir, templateFile);
        if (file.exists()) {
            return file.toURI().toURL();
        }
        return this.getClass().getResource(location);
    }

    private String resolveTemplateFile(String templateDir, String templateFile) {
        if (templateFile.startsWith(templateDir)) {
            if (SystemUtils.IS_OS_WINDOWS) {
                templateDir = templateDir + "/";
            }
            templateFile = StringUtils.replaceOnce(templateFile, templateDir, StringUtils.EMPTY);
        }
        return templateFile;
    }

    public String getCustomTemplateDir() {
        return customTemplateDir;
    }

    public void setCustomTemplateDir(String customTemplateDir) {
        this.customTemplateDir = customTemplateDir;
    }

    public CodegenTemplateLoader customTemplateDir(String customTemplateDir) {
        this.customTemplateDir = customTemplateDir;
        return this;
    }
}
