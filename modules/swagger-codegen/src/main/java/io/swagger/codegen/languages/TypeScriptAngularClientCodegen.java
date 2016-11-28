package io.swagger.codegen.languages;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Map;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Model;

public class TypeScriptAngularClientCodegen extends AbstractTypeScriptClientCodegen {

    @Override
    public String getName() {
        return "typescript-angular";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript AngularJS client library.";
    }

    public TypeScriptAngularClientCodegen() {
        super();
        outputFolder = "generated-code/typescript-angular";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");
        embeddedTemplateDir = templateDir = "TypeScript-Angular";
        apiPackage = "";
        modelPackage = "";
        supportingFiles.add(new SupportingFile("api.ts.mustache", apiPackage().replace('.', File.separatorChar), "api.ts"));
        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/services/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/models/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toModelFilename(String name) {
        return toConventionalFilename(name);
    }

    @Override
    public String toApiFilename(String name) {
        return toConventionalFilename(name + "_api");
    }

    private String toConventionalFilename(String name) {
        return name.toLowerCase().replaceAll("[_]", "-");
    }

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel m = super.fromModel(name, model, allDefinitions);
        m.classFileName = toConventionalFilename(m.classFileName);
        return m;
    }

    private class EnumEntryLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return fragment.replace("-", "");
        }
    }

    private static abstract class CustomLambda implements Mustache.Lambda {
        @Override
        public void execute(Template.Fragment frag, Writer out) throws IOException {
            final StringWriter tempWriter = new StringWriter();
            frag.execute(tempWriter);
            out.write(formatFragment(tempWriter.toString()));
        }

        public abstract String formatFragment(String fragment);
    }
}
