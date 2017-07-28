package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;


// TODO: Move this to another project
public class BaikalCodegen extends JavaPlayFrameworkCodegen {

  public BaikalCodegen() {
    super();
    embeddedTemplateDir = templateDir = "Baikal";
  }

  @Override
  public String getName() {
      return "baikal";
  }

  @Override
  public void processOpts() {
    super.processOpts();
    apiTemplateFiles.remove("newApi.mustache");
    supportingFiles.remove(new SupportingFile("module.mustache", "app", "Module.java"));
  }
}
