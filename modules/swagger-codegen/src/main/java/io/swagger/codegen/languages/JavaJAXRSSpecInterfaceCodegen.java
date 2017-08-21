package io.swagger.codegen.languages;

import java.util.Iterator;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.SupportingFile;

public class JavaJAXRSSpecInterfaceCodegen extends JavaJAXRSSpecServerCodegen {

    public JavaJAXRSSpecInterfaceCodegen() {
        super();

        // Replace default jaxrs api implementation with our interface implementation
        apiTemplateFiles.remove("api.mustache");
        apiTemplateFiles.put("apiInterface.mustache", ".java");

        //TODO: add doc templates
        apiDocTemplateFiles.remove("api_doc.mustache");

        CliOption createPom = CliOption.newBoolean("createPom", "Create pom");
        cliOptions.add(createPom);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        for (Iterator<SupportingFile> iterator = supportingFiles.iterator(); iterator.hasNext(); ) {
            // Remove pom if createPom is anything else thantrue, and never write RestApplication since this is an interface based client
            SupportingFile supportingFile = iterator.next();
            if (supportingFile.templateFile.equals("pom.mustache") && !createPom()) {
                iterator.remove();
            } else if (supportingFile.templateFile.equals("RestApplication.mustache")) {
                iterator.remove();
            }
        }
    }

    private boolean createPom() {
        if (additionalProperties.containsKey("createPom")) {
            return Boolean.valueOf(additionalProperties.get("createPom").toString());
        }
        return false;
    }


    @Override
    public String getName()
    {
        return "jaxrs-interface";
    }

    @Override
    public String getHelp() {
        return "Generates Java JAXRS Interfaces according to JAXRS 2.0 specification for implementation elsewhere.";
    }
}
