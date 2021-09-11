package io.swagger.codegen.v3.templates;

import com.github.jknack.handlebars.io.FileTemplateLoader;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.net.URL;

public class CodegenTemplateLoader extends FileTemplateLoader {
    private String templateDir;
    private String customTemplateDir;

    public CodegenTemplateLoader(String templateDir, String customTemplateDir, String suffix) {
        super(customTemplateDir != null ? customTemplateDir : templateDir, suffix);
        this.templateDir = templateDir;
        this.customTemplateDir = customTemplateDir;
    }

    public URL getResource(String location) throws IOException {
        File file = new File(location);
        if (file.exists()) {
            return file.toURI().toURL();
        }
        location = location.replace(this.customTemplateDir, StringUtils.EMPTY);
        location = this.templateDir + location;

        return this.getClass().getResource(location);
    }
}
