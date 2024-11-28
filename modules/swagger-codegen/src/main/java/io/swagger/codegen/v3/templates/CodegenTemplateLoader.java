package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.io.URLTemplateLoader;

import java.io.File;
import java.io.IOException;
import java.net.URL;

public class CodegenTemplateLoader extends URLTemplateLoader {
    private String templateDir;
    private String customTemplateDir;

    public CodegenTemplateLoader() {
        setSuffix(".mustache");
    }

    @Override
    public String resolve(String uri) {
        if (!uri.endsWith(this.getSuffix())) {
            uri = uri + this.getSuffix();
        }
        File templateFile = new File(uri);
        if (templateFile.exists()) {
            return templateFile.toString();
        }
        templateFile = new File(this.getPrefix() + this.normalize(uri));
        if (templateFile.exists()) {
            return templateFile.toString();
        }
        if (this.customTemplateDir != null) {
            templateFile = new File(this.customTemplateDir, this.normalize(uri));
            if (templateFile.exists()) {
                return templateFile.toString();
            }
        }
        if (getClass().getResource(this.getPrefix() + this.normalize(uri)) != null) {
            return this.getPrefix() + this.normalize(uri);
        }
        return this.templateDir + this.normalize(uri);
    }

    @Override
    public URL getResource(String location) throws IOException {
        if (this.customTemplateDir == null) {
            return this.getClass().getResource(location);
        }
        final File file = new File(location);
        if (file.exists()) {
            return file.toURI().toURL();
        }
        return this.getClass().getResource(location);
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

    public String getTemplateDir() {
        return templateDir;
    }

    public void setTemplateDir(String templateDir) {
        this.templateDir = this.getPrefix() + templateDir;
        if (!this.templateDir.endsWith("/")) {
            this.templateDir = this.templateDir + "/";
        }
    }

    public CodegenTemplateLoader templateDir(String templateDir) {
        setTemplateDir(templateDir);
        return this;
    }
}
