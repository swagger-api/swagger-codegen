package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;

public class KrakenDConfigCodegen extends DefaultCodegen implements CodegenConfig {

  public static final String KRAKEND_DEBUG_DESC = "Enable krakend debugging options";
  public static final String KRAKEND_DEBUG = "krakendDebug";
  public static final String KRAKEND_METERING_DESC = "Enable krakend metering options";
  public static final String KRAKEND_METERING = "krakendMetering";
  public static final String KRAKEND_PORT_DESC = "Default kraken port";
  public static final String KRAKEND_PORT = "krakendPort";

  public KrakenDConfigCodegen() {
    super();
    apiTemplateFiles.put("krakend-config.mustache", ".json");
    embeddedTemplateDir = templateDir = "krakend";
    cliOptions.add(new CliOption(KRAKEND_DEBUG, KRAKEND_DEBUG_DESC));
    cliOptions.add(new CliOption(KRAKEND_METERING, KRAKEND_METERING_DESC));
    cliOptions.add(new CliOption(KRAKEND_PORT, KRAKEND_PORT_DESC));
  }

  @Override
  public CodegenType getTag() {
    return CodegenType.CONFIG;
  }

  @Override
  public String getName() {
    return "krakend";
  }

  @Override
  public String getHelp() {
    return "Generate a krakend configuration json file";
  }
}
