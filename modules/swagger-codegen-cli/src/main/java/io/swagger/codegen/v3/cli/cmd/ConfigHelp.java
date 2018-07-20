package io.swagger.codegen.v3.cli.cmd;

import io.swagger.codegen.v3.CliOption;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConfigLoader;

public class ConfigHelp implements Runnable {
    private String lang;

    public void setLang(String lang) {
        this.lang = lang;
    }

    @Override
    public void run() {
        System.out.println();
        CodegenConfig config = CodegenConfigLoader.forName(lang);
        System.out.println("CONFIG OPTIONS");
        for (CliOption langCliOption : config.cliOptions()) {
            System.out.println("\t" + langCliOption.getOpt());
            System.out.println("\t    "
                    + langCliOption.getOptionHelp().replaceAll("\n", "\n\t    "));
            System.out.println();
        }
    }
}
